let
  pkgs = import <nixpkgs> {};
in 
  pkgs.mkShellNoCC {
    buildInputs = with pkgs; [
      ocaml
      ocamlPackages.ocaml-lsp
      ocamlPackages.ocamlformat
    ];
  }
