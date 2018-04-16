{ mkDerivation, aeson-pretty, base, containers, directory, free
, mtl, optparse-applicative, parsers, stdenv, template-haskell
, text, th-lift-instances, trifecta, turtle, yaml
}:
mkDerivation {
  pname = "stencil";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-pretty base containers directory free mtl
    optparse-applicative parsers template-haskell text
    th-lift-instances trifecta turtle yaml
  ];
  description = "Shareable project templates";
  license = stdenv.lib.licenses.bsd3;
}
