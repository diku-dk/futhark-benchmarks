let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = import ./lib/github.com/diku-dk/lys/build-inputs.nix pkgs;
}
