{ mkDerivation, base, constraints-extras, dependent-map
, dependent-sum, dependent-sum-template, diagrams-contrib
, diagrams-core, diagrams-lib, diagrams-svg, foldl
, foldl-statistics, stdenv, SVGFonts, text
}:
mkDerivation {
  pname = "lgog-digagrams";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraints-extras dependent-map dependent-sum
    dependent-sum-template diagrams-contrib diagrams-core diagrams-lib
    diagrams-svg foldl foldl-statistics SVGFonts text
  ];
  license = stdenv.lib.licenses.bsd3;
}
