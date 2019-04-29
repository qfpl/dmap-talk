{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  inherit (nixpkgs) pkgs;
  drv = import ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv
