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
      inputs.prettyprinter-src.follows = "prettyprinter-src";
    };
    prettyprinter-src = { flake = false; url = github:quchen/prettyprinter?dir=prettyprinter; };
  };

  outputs = { self, nixpkgs, levers
            , kvitable
            , prettyprinter-src
            }: rec
      {
        defaultPackage = levers.eachSystem (s:
          self.packages.${s}.tasty-sugar.default);

        devShell = levers.eachSystem (s: defaultPackage.${s}.env);

        packages = levers.eachSystem (system:
          let mkHaskell = levers.mkHaskellPkg {
                inherit nixpkgs system;
              };
              pkgs = import nixpkgs { inherit system; };
          in rec {
            tasty-sugar = mkHaskell "tasty-sugar" self {
              inherit kvitable prettyprinter;
            };

            prettyprinter = mkHaskell "prettyprinter"
              "${prettyprinter-src}/prettyprinter" {
                adjustDrv = args: drv:
                  pkgs.haskell.lib.overrideCabal
                    (pkgs.haskell.lib.dontCheck
                      (pkgs.haskell.lib.appendConfigureFlags drv
                        [ "--extra-include-dirs=${prettyprinter-macros}" ]))
                    (old: {
                      # The LICENSE.md in prettyprinter is a symlink
                      # to the directory above it, but the src
                      # specification will lose it, so create an
                      # explicit copy instead.
                      preConfigure = ''
                              rm /build/prettyprinter/LICENSE.md
                              cp ${prettyprinter-src}/LICENSE.md /build/prettyprinter/LICENSE.md
                            '';
                          });
                };
            prettyprinter-macros = pkgs.stdenv.mkDerivation {
              pname = "prettyprinter-macros";
              version = "1.7.0";
              phases = [ "installPhase" ];
              src = "${prettyprinter-src}/aux";
              installPhase = ''
                mkdir $out
                cp ${prettyprinter-src}/aux/version-compatibility-macros.h $out/
              '';

            };
          });
      };
}
