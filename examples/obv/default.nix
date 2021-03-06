{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
}:
with import ./.obelisk/impl { inherit system; };
project ./. ({ pkgs, ... }: {
  overrides = self: super: {
    validation = pkgs.haskell.lib.dontCheck super.validation;
    dependent-sum-aeson-orphans = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "dependent-sum-aeson-orphans" (import ./deps/dependent-sum-aeson-orphans) {});
    constraints-extras = super.callCabal2nix "constraints-extras" (import ./deps/constraints-extras) {};
    aeson-gadt-th = super.callCabal2nix "aeson-gadt-th" (import ./deps/aeson-gadt-th) {};
    dependent-monoidal-map = super.callCabal2nix "dependent-monoidal-map" (import ./deps/dependent-monoidal-map) {};
    dependent-sum = super.callCabal2nix "dependent-sum" (import ./deps/dependent-sum) {};
    dependent-sum-template = super.callCabal2nix "dependent-sum-template" (import ./deps/dependent-sum-template) {};
    # dependent-sum = super.callCabal2nix "dependent-sum" "${import ./deps/dependent-sum}/dependent-sum" {};
    # dependent-sum-template = super.callCabal2nix "dependent-sum-template" "${import ./deps/dependent-sum}/dependent-sum-template" {};
    dependent-map = super.callCabal2nix "dependent-map" (import ./deps/dependent-map) {};
    vessel = super.callCabal2nix "vessel" (import ./deps/vessel) {};
    universe-template = pkgs.haskell.lib.doJailbreak super.universe-template;
    reflex = pkgs.haskell.lib.overrideCabal super.reflex (drv: {
      patches = [ ./patches/reflex.cabal.patch ];
    });
    reflex-dom-core = pkgs.haskell.lib.overrideCabal super.reflex-dom-core (drv: {
      patches = [ ./patches/reflex-dom-core.cabal.patch ];
    });
  };
})
