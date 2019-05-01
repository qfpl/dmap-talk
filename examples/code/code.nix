{ mkDerivation, base, constraints-extras, containers, dependent-map
, dependent-monoidal-map, dependent-sum, dependent-sum-template
, stdenv, text, time, vessel
}:
mkDerivation {
  pname = "code";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraints-extras containers dependent-map
    dependent-monoidal-map dependent-sum dependent-sum-template text
    time vessel
  ];
  license = stdenv.lib.licenses.bsd3;
}
