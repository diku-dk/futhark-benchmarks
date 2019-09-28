with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "lys";
    buildInputs = [ pkgconfig SDL2 SDL2_ttf SDL2_mixer SDL2_gfx ocl-icd opencl-headers ];
}
