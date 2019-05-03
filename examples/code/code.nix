{ mkDerivation, base, constraints, constraints-extras, containers
, dependent-map, dependent-monoidal-map, dependent-sum
, dependent-sum-template, reflex, stdenv, text, time, validation
, vessel
}:
mkDerivation {
  pname = "code";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraints constraints-extras containers dependent-map
    dependent-monoidal-map dependent-sum dependent-sum-template reflex
    text time validation vessel
  ];
  license = stdenv.lib.licenses.bsd3;
}
