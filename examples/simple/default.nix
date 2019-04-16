{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self : super: {
      constraints-extras = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "constraints-extras" (import ./deps/constraints-extras.nix) {});
      dependent-sum-aeson-orphans = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "dependent-sum-aeson-orphans" (import ./deps/dependent-sum-aeson-orphans.nix) {});
    };
  };
  drv = modifiedHaskellPackages.callPackage ./ylj19.nix {};
in
  drv
