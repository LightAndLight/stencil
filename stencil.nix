{ mkDerivation, base, containers, directory, free, mtl
, optparse-applicative, parsers, stdenv, template-haskell, text
, th-lift-instances, trifecta
}:
mkDerivation {
  pname = "stencil";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory free mtl optparse-applicative parsers
    template-haskell text th-lift-instances trifecta
  ];
  executableHaskellDepends = [ base optparse-applicative trifecta ];
  description = "Shareable project templates";
  license = stdenv.lib.licenses.bsd3;
}
