{
  # $ nix develop
  # $ nix build
  # $ nix develop .#tasty-sugar.ghc884.env
  # $ nix build  .#tasty-sugar.ghc884.default

  description = "Haskell tasty-sugar (Search Using Golden Answer References) testing tool";

  nixConfig.bash-prompt-suffix = "tasty-sugar} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kvitable = {
      url = "/home/kquick/Projects/kvitable/kvitable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
  };

  outputs = { self, nixpkgs, levers
            , kvitable
            }:
              let shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
                    { buildInputs = old.buildInputs ++ adds pkgs; });
                  shellPkgs = pkgs: [
                    pkgs.cabal-install
                  ];
              in
                rec
      {
        defaultPackage = levers.eachSystem (s:
          self.packages.${s}.tasty-sugar.default);

        devShell = levers.eachSystem (s:
          let pkgs = import nixpkgs { system=s; };
          in shellWith pkgs shellPkgs defaultPackage.${s}.env);

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
          in rec {
            tasty-sugar = mkHaskell "tasty-sugar" self {
              inherit kvitable prettyprinter;
            };

          });
      };
}
