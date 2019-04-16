let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-monoidal-map-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./github.json;
    dependent-monoidal-map = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "dependent-monoidal-map";
      inherit (dependent-monoidal-map-info-pinned) rev sha256;
    };
  };
in
  sources.dependent-monoidal-map

