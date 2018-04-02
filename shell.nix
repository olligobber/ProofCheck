{ pkgs ? import <nixpkgs> {} }:

    with pkgs;
    stdenv.mkDerivation {
        name = "ProofCheck";
        buildInputs = with elmPackages ; [
            elm-make elm-package elm-repl
        ];
    }
