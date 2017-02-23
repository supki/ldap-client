{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: let
  inherit (nixpkgs) pkgs;
  haskell = pkgs.haskell.packages.${compiler};

  ghc = haskell.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover ps.hlint ps.ghc-mod
  ]);
  npm = import ./npm {};

  this = import ./default.nix { inherit nixpkgs compiler; };
in
  pkgs.stdenv.mkDerivation rec {
    name = this.pname;
    buildInputs = [
      ghc
      haskell.cabal-install
      npm.nodePackages.ldapjs
    ] ++ this.env.buildInputs;
    shellHook = ''
      ${this.env.shellHook}
      cabal configure --enable-tests --package-db=$NIX_GHC_LIBDIR/package.conf.d
    '';
  }
