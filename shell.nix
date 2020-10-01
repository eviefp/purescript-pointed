let
  tooling = import ./default.nix;
in
  tooling.purescript.mkShell
