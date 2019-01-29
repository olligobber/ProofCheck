{ pkgs ? import <nixpkgs> {} }:

    with pkgs;
    stdenv.mkDerivation {
        name = "ProofCheck";
        buildInputs = with elmPackages ; [
            elm-make elm-reactor elm-package elm-repl
        ];
    }

# TODO update to include sass
