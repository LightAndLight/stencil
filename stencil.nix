{ mkDerivation, base, containers, directory, free, mtl, parsers
, stdenv, template-haskell, text, th-lift-instances, trifecta
}:
mkDerivation {
  pname = "stencil";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers directory free mtl parsers template-haskell text
    th-lift-instances trifecta
  ];
  testHaskellDepends = [ base trifecta ];
  description = "Shareable project templates";
  license = stdenv.lib.licenses.bsd3;
}
