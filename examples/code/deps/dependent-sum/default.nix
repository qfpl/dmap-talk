let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-sum-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./github.json;
    dependent-sum = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "dependent-sum";
      inherit (dependent-sum-info-pinned) rev sha256;
    };
  };
in
  sources.dependent-sum

