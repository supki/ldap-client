{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc966
, stack ? pkgs.stack
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    nodejs
    stack
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}
  '';
}
