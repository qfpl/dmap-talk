let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-sum-template-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./github.json;
    dependent-sum-template = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "dependent-sum-template";
      inherit (dependent-sum-template-info-pinned) rev sha256;
    };
  };
in
  sources.dependent-sum-template

