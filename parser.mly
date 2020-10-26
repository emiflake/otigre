(* Identifiers *)
%token <string> ID

%token <string> STRING

(* Literal *)
%token <int> INT

(* Special *)
%token EOF

(* Keywords *)
%token ARRAY BREAK DO ELSE END FOR FUNCTION IF IN LET NIL OF THEN TO TYPE VAR WHILE

(* Punctuation *)
%token COMMA COLON SEMICOLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token DOT

(* Operators *)
%token PLUS MINUS TIMES DIVIDE

%token EQ NEQ LT LTE GT GTE
%token AND OR

%token ASSIGN

(* Precedence of operators *)
%nonassoc DO
%nonassoc THEN
%nonassoc ELSE
%left ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ GT LT GTE LTE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <Ast.expr> main

%%

main:
    | ex = expr EOF { ex }

expr:
    | str = STRING
    { Ast.StringConstant str }

    | integer = INT
    { Ast.IntegerConstant integer }

    | NIL
    { Ast.Nil }

    | lv = lvalue
    { Ast.LValue lv }

    | e = binop_expression
    { e }

    | lv = lvalue; ASSIGN; e = expr
    { Ast.Assign(lv, e) }

    | id = ID; LPAREN; es = separated_list(COMMA, expr); RPAREN
    { Ast.FunctionCall(id, es) }

    | LPAREN; es = separated_list(SEMICOLON, expr); RPAREN
    { Ast.ExprSeq(es) }

    | id = ID; LBRACKET;
      assignments = separated_list(COMMA, k = ID; EQ; v = expr { (k, v) });
      RBRACKET
    { Ast.RecordConstructor(id, assignments) }

    (* TODO: implement array constructor, not possible due to ambiguous grammar up to recursion *)
    (* | id = ID; LBRACE; sz = expr; RBRACE; OF; v = expr; END
     * { Ast.ArrayConstructor(id, sz, v) } *)

    | IF c = expr; THEN; t = expr
    { Ast.IfThenElse(c, t, Ast.Nil) } (* NOTE: Defaults to Nil on false branch*)

    | IF c = expr; THEN; t = expr; ELSE; f = expr
    { Ast.IfThenElse(c, t, f) }

    | WHILE c = expr; DO; l = expr;
    { Ast.While(c, l) }

    | FOR id = ID; ASSIGN; low = expr; TO; high = expr; DO; l = expr
    { Ast.For(id, low, high, l) }

    | BREAK;
    { Ast.Break }

    | LET; decls = list(declaration); IN; es = separated_list(SEMICOLON, expr); END
    { Ast.LetBinding(decls, es) }


type_declaration:
    | TYPE; id = ID; EQ; ty = typ { Ast.TypeDeclaration(id, ty) }

typ:
    | LBRACKET; type_fields = separated_list(COMMA, type_field); RBRACKET
    { Ast.TypeRecord(type_fields) }

    | id = ID
    { Ast.TypeAlias(id) }

    | ARRAY OF; id = ID
    { Ast.TypeArray(id) }

type_field:
    | id = ID; COLON; ty = ID; { Ast.TypeField(id, ty) }


variable_declaration:
    | VAR; id = ID; ASSIGN; e = expr
    { Ast.VariableDeclaration(id, Option.None, e) }

    | VAR; id = ID; COLON; ty = ID; ASSIGN; e = expr
    { Ast.VariableDeclaration(id, Option.Some(ty), e) }

function_declaration:
    | FUNCTION; id = ID; LPAREN; type_fields = separated_list(COMMA, type_field); RPAREN; EQ; body = expr;
    { Ast.FunctionDeclaration(id, type_fields, Option.None, body) }

    | FUNCTION; id = ID; LPAREN; type_fields = separated_list(COMMA, type_field); RPAREN; COLON; typ_id = ID; EQ; body = expr;
    { Ast.FunctionDeclaration(id, type_fields, Option.Some(typ_id), body) }

declaration:
    | ty_decl = type_declaration { ty_decl }
    | var_decl = variable_declaration { var_decl }
    | fun_decl = function_declaration { fun_decl }

%inline binary_expr(Lhs, Op, Rhs):
   lhs = Lhs; op = Op; rhs = Rhs
   { Ast.BinOp(lhs, op, rhs) }

%inline binop(Op):
   e = binary_expr(expr, Op, expr)
   { e }

binop_expression:
    | e = binop( PLUS { Ast.Add }
               | MINUS { Ast.Subtract }) { e }

    | e = binop( TIMES { Ast.Multiply }
               | DIVIDE { Ast.Divide }) { e }

    | e = binop( EQ { Ast.Eq }
               | NEQ { Ast.Neq }
               | GT { Ast.Gt }
               | LT { Ast.Lt }
               | GTE { Ast.Gte }
               | LTE { Ast.Lte }
               ) { e }

    | e = binop( AND { Ast.And }) { e }
    | e = binop( OR { Ast.Or }) { e }

    | MINUS; e = expr { Ast.Negate e } %prec UMINUS

lvalue:
    | id = ID { Ast.LValueId id }
    | lv = lvalue; DOT; id = ID { Ast.LValueAt (lv, id) }
    | lv = lvalue; LBRACE; e = expr; RBRACE { Ast.LValueIndex (lv, e) }
