{
  # $ nix develop
  # $ nix build
  # $ nix develop .#tasty-sugar.ghc884.env
  # $ nix build  .#tasty-sugar.ghc884.default

  description = "Haskell tasty-sugar (Search Using Golden Answer References) testing tool";

  nixConfig.bash-prompt-suffix = "tasty-sugar} ";

  inputs = {
    # nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    nixpkgs.url = "github:nixos/nixpkgs/23.05";
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hedgehog-src = {
      url = "github:hedgehogqa/haskell-hedgehog";
      flake = false;
    };
    kvitable = {
      url = "github:kquick/kvitable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
    optparse-applicative-src = {
      url = "github:pcapriotti/optparse-applicative/0.18.1";
      flake = false;
    };
    tasty-src = {
      url = "github:UnkindPartition/tasty/core-1.4.3";
      flake = false;
    };
    tasty-hedgehog-src = {
      url = "github:qfpl/tasty-hedgehog";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers
            , hedgehog-src
            , optparse-applicative-src
            , kvitable
            , tasty-src
            , tasty-hedgehog-src
            }:
     let shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
           { buildInputs = old.buildInputs ++ adds pkgs; });
         shellPkgs = pkgs: [ pkgs.cabal-install ];
     in
     rec
      {
        devShell = levers.eachSystem (s:
          let pkgs = import nixpkgs { system=s; };
          in shellWith pkgs shellPkgs packages.${s}.default.env);

        devShells =
          let oneshell = s: n:
                let pkgs = import nixpkgs { system=s; };
                in levers.variedTargets
                  { ghcver = levers.validGHCVersions pkgs.haskell.compiler; }
                  ( { ghcver, ... } @ vargs:
                    shellWith pkgs shellPkgs
                      (self.packages.${s}.${n}.${ghcver}.env)
                  );
          in
            levers.eachSystem
                (s:
                  let pkgs = import nixpkgs { system=s; };
                      names = builtins.attrNames (self.packages.${s});
                  in pkgs.lib.genAttrs names (oneshell s)
                ) ;

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg {
                inherit nixpkgs system;
              };
              pkgs = import nixpkgs { inherit system; };
              wrap = levers.pkg_wrapper system pkgs;
              haskellAdj = drv:
                with (pkgs.haskell).lib;
                dontHaddock (dontCheck (dontBenchmark (drv)));
          in rec {
            default = tasty-sugar;
            TESTS = wrap "tasty-sugar-TESTS" [ tasty-sugar-test ];
            DOC = wrap "tasty-sugar-DOC" [ tasty-sugar-doc ];
            tasty-sugar = mkHaskell "tasty-sugar" self {
              inherit kvitable optparse-applicative tasty;
              adjustDrv = args: haskellAdj;
            };
            tasty-sugar-test = mkHaskell "tasty-sugar-test" self {
              inherit hedgehog
                kvitable optparse-applicative tasty tasty-hedgehog tasty-hunit;
              adjustDrv = args: drv: pkgs.haskell.lib.doCheck (haskellAdj drv);
            };
            tasty-sugar-doc = mkHaskell "tasty-sugar-doc" self {
              inherit kvitable optparse-applicative tasty;
              adjustDrv = args: drv:
                with pkgs.haskell.lib; dontCheck (dontBenchmark drv);
            };
            hedgehog = mkHaskell "hedgehog" "${hedgehog-src}/hedgehog" {
              adjustDrv = args: haskellAdj;
            };
            optparse-applicative = mkHaskell "optparse-applicative" optparse-applicative-src {
              adjustDrv = args: haskellAdj;
            };
            tasty = mkHaskell "tasty" "${tasty-src}/core" {
              inherit optparse-applicative;
              adjustDrv = args: haskellAdj;
            };
            tasty-hunit = mkHaskell "tasty-hunit" "${tasty-src}/hunit" {
              inherit tasty;
              adjustDrv = args: haskellAdj;
            };
            tasty-hedgehog = mkHaskell "tasty-hedgehog" tasty-hedgehog-src {
              inherit hedgehog tasty;
              adjustDrv = args: haskellAdj;
            };
          });
      };
}
