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
      url = "github:kquick/kvitable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
    };
    parameterized-utils-src = {
      url = "github:GaloisInc/parameterized-utils";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers
            , parameterized-utils-src
            , kvitable
            }:
    rec
      {
        apps = levers.eachSystem (s:
          rec
          {
            test-passthru-ascii = {
              type = "app";
              program = "${self.packages.${s}.tasty-sugar}/bin/test-passthru-ascii";
            };
            default = test-passthru-ascii;
          });

        devShells = levers.haskellShells
          { inherit nixpkgs;
            flake = self;
            defaultPkg = "tasty-sugar";
            # additionalPackages = pkgs.haskell.packages.ghc8107.profiteur
          };

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg { inherit nixpkgs system; };
              pkgs = import nixpkgs { inherit system; };
          in rec {
            default = tasty-sugar;
            tasty-sugar = mkHaskell "tasty-sugar" self { inherit kvitable; };
          });
      };
}
