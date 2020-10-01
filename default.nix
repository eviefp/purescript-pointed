let
  sources = import ./nix/sources.nix;
  tooling = import sources.nix-tooling;
in
  tooling
