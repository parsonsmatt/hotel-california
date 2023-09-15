{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";

    flake-utils.url = "github:numtide/flake-utils";

    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
  };

  outputs = { all-cabal-hashes, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: super: {
          inherit all-cabal-hashes;

          cabal2nix-unwrapped =
            self.haskell.lib.justStaticExecutables
              super.haskellPackages.cabal2nix;

          haskellPackages = super.haskellPackages.override (old: {
            overrides =
              self.lib.fold
                self.lib.composeExtensions
                (old.overrides or (_: _: { }))
                [ (self.haskell.lib.packageSourceOverrides {
                    hotel-california = ./.;

                    hs-opentelemetry-api = "0.1.0.0";
                    hs-opentelemetry-exporter-otlp = "0.0.1.5";
                    hs-opentelemetry-propagator-b3 = "0.0.1.1";
                    hs-opentelemetry-propagator-w3c = "0.0.1.3";
                    hs-opentelemetry-sdk = "0.0.3.6";
                    hs-opentelemetry-utils-exceptions = "0.2.0.0";

                    optparse-applicative = "0.18.1.0";
                  })

                  (hself: hsuper: {
                    prettyprinter-ansi-terminal =
                      self.haskell.lib.dontCheck
                        hsuper.prettyprinter-ansi-terminal;
                  })
                ];
          });
        };

        config = { };

        overlays = [ overlay ];

        pkgs = import nixpkgs { inherit config overlays system; };

      in
        { packages.default = pkgs.haskellPackages.hotel-california;

          devShells.default = pkgs.haskellPackages.hotel-california.env;
        }
    );
}
