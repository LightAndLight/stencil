{ mkDerivation, aeson-pretty, base, bytestring, containers
, directory, free, mtl, optparse-applicative, parsers, process
, stdenv, template-haskell, text, th-lift-instances, trifecta, yaml
}:
mkDerivation {
  pname = "stencil";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-pretty base bytestring containers directory free mtl
    optparse-applicative parsers process template-haskell text
    th-lift-instances trifecta yaml
  ];
  description = "Shareable project templates";
  license = stdenv.lib.licenses.bsd3;
}
