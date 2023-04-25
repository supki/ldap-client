{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc8107
, stack ? pkgs.stack
}:

pkgs.mkShell rec {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    nodejs
    stack
    zlib
  ];

  shellHook = ''
  '';
}
