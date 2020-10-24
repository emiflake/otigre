{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [ ] ++ (with pkgs.ocamlPackages; [
    ocaml
    core
    core_extended
    findlib
    utop
    merlin
    ocp-indent
    menhir
    merlin
    dune

  ]);
}
