{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false, withHoogle ? true }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.;

  hei-tools = hpkgs: with hpkgs; 
    [ Cabal_2_4_1_0 apply-refact hasktags hlint ];

  # Enable Hoogle and packages
  overrides = self: super: {
    ghc = super.ghc // { withPackages = if withHoogle
                                          then super.ghc.withHoogle
                                          else super.ghc.withPackages; };
    ghcWithPackages = lp: self.ghc.withPackages (hpkgs: hei-tools hpkgs ++ lp hpkgs);
  };

  selPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages = selPackages.override { inherit overrides; };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
