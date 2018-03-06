{ mkDerivation, base, containers, hdom-api, lucid, monad-supply
, stdenv, text
}:
mkDerivation {
  pname = "hdom-lucid";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hdom-api lucid monad-supply text
  ];
  description = "lucid ToHTML instance for HDOM";
  license = stdenv.lib.licenses.bsd3;
}
