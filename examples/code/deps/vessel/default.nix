let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    vessel-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./github.json;
    vessel = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "vessel";
      inherit (vessel-info-pinned) rev sha256;
    };
  };
in
  sources.vessel

