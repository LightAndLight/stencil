{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = import ./default.nix { inherit nixpkgs compiler; };

in

  if pkgs.lib.inNixShell then drv.env else drv
