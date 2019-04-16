let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    dependent-map-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./github.json;
    dependent-map = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "mokus0";
      repo = "dependent-map";
      inherit (dependent-map-info-pinned) rev sha256;
    };
  };
in
  sources.dependent-map

