{ mkDerivation, base, containers, directory, free, mtl
, optparse-applicative, parsers, stdenv, template-haskell, text
, th-lift-instances, trifecta, turtle
}:
mkDerivation {
  pname = "stencil";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory free mtl optparse-applicative parsers
    template-haskell text th-lift-instances trifecta turtle
  ];
  executableHaskellDepends = [
    base optparse-applicative text trifecta turtle
  ];
  description = "Shareable project templates";
  license = stdenv.lib.licenses.bsd3;
}
