with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "lys";
    buildInputs = [ pkgconfig SDL2 SDL2_ttf ocl-icd opencl-headers ];
}
