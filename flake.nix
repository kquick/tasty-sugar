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
      inputs.microlens-src.follows = "microlens-src";
      inputs.named-text.follows = "named-text";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      inputs.sayable.follows = "sayable";
      inputs.tasty-checklist.follows = "tasty-checklist";
    };
    microlens-src = {
      url = "github:stevenfontanella/microlens";
      flake = false;
    };
    named-text = {
      url = "github:kquick/named-text";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.microlens-src.follows = "microlens-src";
      inputs.sayable.follows = "sayable";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
      inputs.tasty-checklist.follows = "tasty-checklist";
    };
    parameterized-utils-src = {
      url = "github:GaloisInc/parameterized-utils";
      flake = false;
    };
    sayable = {
      url = "github:kquick/sayable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
    tasty-checklist = {
      url = "github:kquick/tasty-checklist";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.microlens-src.follows = "microlens-src";
      inputs.parameterized-utils-src.follows = "parameterized-utils-src";
    };
  };

  outputs = { self, nixpkgs, levers
            , microlens-src
            , named-text
            , parameterized-utils-src
            , kvitable
            , sayable
            , tasty-checklist
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
