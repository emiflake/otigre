type operator =
    Add
  | Subtract
  | Multiply
  | Divide
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  [@@deriving show]

type ident = string
  [@@deriving show]

type expr =
    StringConstant of string
  | IntegerConstant of int
  | Nil
  | LValue of lvalue
  | Negate of expr
  | BinOp of expr * operator * expr
  | Assign of lvalue * expr
  | FunctionCall of ident * expr list
  | ExprSeq of expr list
  | LetBinding of decl list * expr list
  | RecordConstructor of ident * field list
  | ArrayConstructor of ident * expr * expr
  | IfThenElse of expr * expr * expr
  | While of expr * expr
  | For of ident * expr * expr * expr
  | Break
  [@@deriving show]

 and lvalue =
    LValueId of string
  | LValueAt of lvalue * string
  | LValueIndex of lvalue * expr

 and typ =
    TypeAlias of ident
  | TypeRecord of typ_field list
  | TypeArray of ident
  [@@deriving show]

 and typ_field = TypeField of ident * ident
  [@@deriving show]

 and field = ident * expr
  [@@deriving show]

 and decl =
    TypeDeclaration of ident * typ
  | VariableDeclaration of ident * ident option * expr
  | FunctionDeclaration of ident * typ_field list * ident option * expr
  [@@deriving show]

let show_op op =
  match op with
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "&"
  | Or -> "|"

(* Deprecated pretty printing rules, using ppx_deriving.show instead



let show_list sep show_elem list =
  let rec go xs =
    match xs with
    | [] -> ""
    | e :: [] -> show_elem e
    | e :: es -> Printf.sprintf "%s%s %s" (show_elem e) sep (go es)
  in
  Printf.sprintf "[%s]" (go list)

let rec show_expr expr =
  match expr with
  | StringConstant str ->
    Printf.sprintf "String(\"%s\")" str

  | IntegerConstant v ->
    Printf.sprintf "Int(%d)" v

  | Nil ->
    Printf.sprintf "Nil"

  | LValue lv ->
    Printf.sprintf "LValue(%s)" (show_lvalue lv)

  | Negate ex ->
    Printf.sprintf "Negate(%s)" (show_expr ex)

  | BinOp (lhs, op, rhs) ->
    Printf.sprintf "BinOp(%s, %s, %s)" (show_expr lhs) (show_op op) (show_expr rhs)

  | Assign (lv, value) ->
    Printf.sprintf "Assign(%s, %s)" (show_lvalue lv) (show_expr value)

  | FunctionCall (id, expr_list) ->
    Printf.sprintf "FunctionCall(%s, %s)" id (show_list "," show_expr expr_list)

  | ExprSeq expr_list ->
    Printf.sprintf "ExprSeq(%s)" (show_list ";" show_expr expr_list)

  | LetBinding (decl_list, expr_list) ->
    Printf.sprintf "LetBinding(%s, %s)" (show_list "," show_decl decl_list) (show_list ";" show_expr expr_list)

  | RecordConstructor (id, field_list) ->
    let show_field field =
      match field with
        (k, v) -> Printf.sprintf "%s = %s" k (show_expr v)
    in
    Printf.sprintf "RecordConstructor(%s, %s)" id
      (show_list "," show_field field_list)

 and show_lvalue lv =
   match lv with
   | LValueId id ->
     Printf.sprintf "%s" id

   | LValueAt (lv, id) ->
     Printf.sprintf "%s.%s" (show_lvalue lv) id

   | LValueIndex (lv, ex) ->
     Printf.sprintf "%s[%s]" (show_lvalue lv) (show_expr ex)

 and show_typ typ =
   match typ with
   | TypeAlias ident -> Printf.sprintf "TypeAlias(%s)" ident
   | TypeRecord types -> show_list "," show_typ_field types
   | TypeArray ident -> Printf.sprintf "TypeArray(%s)" ident

 and show_typ_field typ_field =
   match typ_field with
   | TypeField (typ, binding) -> Printf.sprintf "TypeField(%s, %s)" typ binding

 and show_decl decl =
   match decl with
   | TypeDeclaration (ident, typ) ->
     Printf.sprintf "TypeDeclaration(%s, %s)" ident (show_typ typ)

   | VariableDeclaration (ident, expr) ->
     Printf.sprintf "VariableDeclaration(%s, %s)" ident (show_expr expr)

   | VariableDeclarationTyped (ident, typ_ident, expr) ->
     Printf.sprintf "VariableDeclarationTyped(%s, %s, %s)" ident typ_ident (show_expr expr)
*)
