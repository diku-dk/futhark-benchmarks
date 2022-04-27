let
  sources = import ../../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    zlib
    python39
    python39Packages.numpy
    python39Packages.pyopencl
    python39Packages.pysdl2
  ];
}
