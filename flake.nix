{
  description = "A chat bridge of your own: every integration's chat on remux's wire, and every event on a Unix socket";

  nixConfig = {
    extra-substituters = [ "https://anmonteiro.nix-cache.workers.dev" ];
    extra-trusted-public-keys = [ "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=" ];
  };

  inputs = {
    # OCaml and its packages come from nix-ocaml's overlays, over nixpkgs.
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5;
        deps = with ocamlPackages; [
          eio
          eio_main
          tls
          tls-eio
          ca-certs
          x509
          cohttp-eio
          http
          uri
          domain-name
          ipaddr
          mirage-crypto-rng
          yojson
          digestif
          base64
        ];
        bridge = ocamlPackages.buildDunePackage {
          pname = "bridge";
          version = "0.1.0";
          src = ./.;
          buildInputs = deps;
        };
      in {
        packages.default = bridge;
        apps.default = flake-utils.lib.mkApp { drv = bridge; name = "bridge"; };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ bridge ];
          packages = (with ocamlPackages; [ ocaml-lsp ocamlformat utop ]);
        };
      });
}
