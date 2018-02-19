{ mkDerivation, aeson, aeson-pretty, base, bytestring, lens, stdenv
, text, wreq
}:
mkDerivation {
  pname = "phabricator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring lens text wreq
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring lens text wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}
