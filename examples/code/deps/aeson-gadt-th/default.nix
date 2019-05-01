let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    aeson-gadt-th-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./github.json;
    aeson-gadt-th = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "aeson-gadt-th";
      inherit (aeson-gadt-th-info-pinned) rev sha256;
    };
  };
in
  sources.aeson-gadt-th

