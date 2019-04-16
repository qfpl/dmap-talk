let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    constraints-extras-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./constraints-extras.json;
    constraints-extras = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "constraints-extras";
      inherit (constraints-extras-info-pinned) rev sha256;
    };
  };
in
  sources.constraints-extras

