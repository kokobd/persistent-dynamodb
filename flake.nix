{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev:
            rec {
              dynamodb =
                prev.stdenv.mkDerivation {
                  name = "dynamodb";
                  src = builtins.fetchurl {
                    url = "https://s3.ap-northeast-1.amazonaws.com/dynamodb-local-tokyo/dynamodb_local_latest.tar.gz";
                    sha256 = "sha256:0xim1g97m0kklkqk9573aizac3c70r8vjcv14wn8al3cz7b68da3";
                  };
                  buildPhase = ''
                    mkdir -p $out
                    cd $out
                    tar -xf $src
                  '';
                  installPhase =
                    let jre = prev.jre_minimal.override {
                      modules = [
                        "java.base"
                        "java.logging"
                        "java.xml"
                        "java.desktop"
                        "java.management"
                      ];
                    }; in
                    ''
                      mkdir -p $out/bin
                      cat > $out/bin/dynamodb <<EOF
                        ${jre}/bin/java -Djava.library.path=$out/DynamoDBLocal_lib \
                          -jar $out/DynamoDBLocal.jar "\$@"
                      EOF
                      chmod +x $out/bin/dynamodb
                    '';
                  checkPhase = ''
                    $out/bin/dynamodb -help
                  '';
                };

              persistent-dynamodb = final.haskell-nix.project' {
                src = ./.;
                name = "persistent-dynamodb";
                compiler-nix-name = "ghc925"; # Version of GHC to use

                shell = {
                  additional = pkgs:
                    # pkgs contains some non-package attributes, so we need rigorous checks to filter them out 
                    builtins.attrValues
                      (
                        prev.lib.filterAttrs
                          (
                            _: pkg:
                              pkg != null
                              && (pkg.isLocal or false)
                              && !pkg.isProject
                          )
                          pkgs
                      );
                  tools = {
                    cabal = "latest";
                    haskell-language-server = {
                      version = "latest";
                      configureArgs = ''--constraint "haskell-language-server -dynamic"'';
                    };
                    hlint = "latest";
                    cabal-fmt = "latest";
                    ormolu = "latest";
                  };
                  buildInputs = with prev; [
                    rnix-lsp
                    nixpkgs-fmt
                    dynamodb
                  ];
                  withHoogle = true;
                };
              };
            })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.persistent-dynamodb.flake {
          crossPlatforms = p: [ ];
        };
      in
      flake // rec {
        legacyPackages = pkgs;
        packages.default = flake.packages."persistent-dynamodb:lib:persistent-dynamodb";
        checks.default = flake.checks."persistent-dynamodb:test:persistent-dynamodb-test".overrideAttrs
          (finalAttrs: prevAttrs: {
            buildPhase = ''
              export PATH=${pkgs.dynamodb}/bin:$PATH
            '' + prevAttrs.buildPhase;
          });
      });

  nixConfig = {
    allow-import-from-derivation = "true";
  };
}
