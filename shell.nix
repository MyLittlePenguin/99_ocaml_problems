let
  pkgs = import <nixpkgs> {};
in 
  pkgs.mkShellNoCC {
    buildInputs = with pkgs; [
      dune_3
      ocaml
      ocamlPackages.ocaml-lsp
      ocamlPackages.ocamlformat
    ];
  }
