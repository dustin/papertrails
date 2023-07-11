{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    stack
    stylish-haskell
    hlint
    niv
    ghcid
    haskell-language-server
  ];

  shellHook = ''
    # ...
  '';
}
