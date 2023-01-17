let pkgs = import <nixpkgs> { };
in with pkgs;
mkShell {
  buildInputs = [
    cabal-install
    ghcid
    (ghc.withPackages (x: with x; [ hoogle ]))
    (pkgs.haskell-language-server.override {
      supportedGhcVersions = [ "902" ];
    })
  ];

  shellHook = ''
    set -o vi
    alias v=vim
    git status
  '';

}
