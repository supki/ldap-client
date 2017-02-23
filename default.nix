{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: let
  ghc = nixpkgs.pkgs.haskell.packages.${compiler};
  npm = import ./npm {};
in
  ghc.callPackage ./package.nix {
    mkDerivation = args: ghc.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ npm.nodePackages.ldapjs ];
    });
  }
