{ mkDerivation, base, stdenv, text }:
mkDerivation {
  pname = "stencil";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  description = "Shareable project templates";
  license = stdenv.lib.licenses.bsd3;
}
