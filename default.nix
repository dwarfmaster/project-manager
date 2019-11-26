{ mkDerivation, base, containers, dhall, directory, filepath
, prettyprinter, relude, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "project-manager";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers dhall directory filepath prettyprinter relude
    template-haskell text
  ];
  description = "Helper software to manage my projects descriptions";
  license = stdenv.lib.licenses.mit;
  
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$(readlink -f $(whereis ghc | cut -d' ' -f 2) | xargs dirname)/../share/doc/hoogle/index.html"
  '';
}
