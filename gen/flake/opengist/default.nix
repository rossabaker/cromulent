{ inputs, lib, ... }:

{
  perSystem = { config, self', inputs', pkgs, system, ... }:
    let
      version = "1.5.3";

      assets =
        pkgs.buildNpmPackage rec {
          pname = "opengist-assets";
          inherit version;
          src = inputs.opengist;
          npmDepsHash = "sha256-I7u9FkqkMaECozLxOJED95B3cT9nvHk0IMu6QJo+Cqs=";
          installPhase = "
            mkdir -p $out
            cp -a public/* $out
          ";
        };
    in {
      packages.opengist = pkgs.buildGoModule {
        pname = "opengist";
        inherit version;
        src = inputs.opengist;

        vendorHash = "sha256-tB75KHgcAEuwyMArTAvI4t8tWVSh+pyZ0e+lgt4KWSQ=";

        tags = [ "fs_embed" ];

        preBuild = ''
          cp -a ${assets}/* public
        '';

        preCheck = ''
          mkdir -p test-home
          export HOME=$PWD/test-home
        '';

        nativeCheckInputs = [ pkgs.git ];

        meta = with lib; {
          description = "Self-hosted pastebin powered by Git, open-source alternative to Github Gist.";
          homepage = "https://github.com/thomiceli/opengist";
          license = licenses.agpl3;
        };
      };
    };
}
