let
  tooling = import ./default.nix;
  pkgs = tooling.pkgs;
  ps = tooling.purescript;
in
  pkgs.mkShell {
    buildInputs = [
      ps.nodejs
      ps.purs
      ps.spago
      ps.pscid
      ps.spago2nix
      pkgs.nodePackages.bower
    ];
  }
