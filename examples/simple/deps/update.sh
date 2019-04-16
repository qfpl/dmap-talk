#!/usr/bin/env bash

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/obsidiansystems/constraints-extras > constraints-extras.json
nix-prefetch-git https://github.com/obsidiansystems/dependent-sum-aeson-orphans > dependent-sum-aeson-orphans.json
