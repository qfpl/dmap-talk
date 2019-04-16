{ mkDerivation, aeson, aeson-gadt-th, base, constraints
, constraints-extras, dependent-map, dependent-sum
, dependent-sum-aeson-orphans, dependent-sum-template, lens, stdenv
}:
mkDerivation {
  pname = "ylj19";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-gadt-th base constraints constraints-extras
    dependent-map dependent-sum dependent-sum-aeson-orphans
    dependent-sum-template lens
  ];
  license = stdenv.lib.licenses.bsd3;
}
