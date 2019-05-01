#!/usr/bin/env bash
nix-prefetch-git git@github.com:reflex-frp/reflex-platform.git > reflex-platform/github.json
nix-prefetch-git git@github.com:obsidiansystems/vessel.git refs/heads/ryantrinkle/db > vessel/github.json
nix-prefetch-git git@github.com:obsidiansystems/constraints-extras.git > constraints-extras/github.json
nix-prefetch-git git@github.com:obsidiansystems/dependent-sum-aeson-orphans.git > dependent-sum-aeson-orphans/github.json
nix-prefetch-git git@github.com:obsidiansystems/dependent-monoidal-map.git refs/heads/develop > dependent-monoidal-map/github.json
nix-prefetch-git git@github.com:obsidiansystems/dependent-sum.git refs/heads/master > dependent-sum/github.json
nix-prefetch-git git@github.com:obsidiansystems/dependent-sum-template.git refs/heads/master > dependent-sum-template/github.json
nix-prefetch-git git@github.com:mokus0/dependent-map.git refs/heads/master > dependent-map/github.json
nix-prefetch-git git@github.com:obsidiansystems/aeson-gadt-th.git > aeson-gadt-th/github.json
