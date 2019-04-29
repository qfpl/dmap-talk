{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      foldl-statistics = pkgs.haskell.lib.doJailbreak super.foldl-statistics;
    };
  };

  drv = modifiedHaskellPackages.callPackage ./lgog-diagrams.nix {};
in
  drv
