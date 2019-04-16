let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-sum-aeson-orphans-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./dependent-sum-aeson-orphans.json;
    dependent-sum-aeson-orphans = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "dependent-sum-aeson-orphans";
      inherit (dependent-sum-aeson-orphans-info-pinned) rev sha256;
    };
  };
in
  sources.dependent-sum-aeson-orphans

