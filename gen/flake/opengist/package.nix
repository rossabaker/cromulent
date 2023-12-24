{ pkgs, lib, src, ... }:

pkgs.buildGoModule rec {
  pname = "opengist";
  version = "1.5.3";

  inherit src;

  assets = pkgs.buildNpmPackage {
    inherit src version;
    pname = "opengist-assets";
    npmDepsHash = "sha256-I7u9FkqkMaECozLxOJED95B3cT9nvHk0IMu6QJo+Cqs=";
    installPhase = ''
      mkdir -p $out
      cp -r public $out
    '';
  };
  nativeBuildInputs = [ assets ];
  preBuild = ''
    cp -rT ${assets} .
  '';

  vendorHash = "sha256-tB75KHgcAEuwyMArTAvI4t8tWVSh+pyZ0e+lgt4KWSQ=";

  tags = [ "fs_embed" ];

  nativeCheckInputs = [ pkgs.git ];
  preCheck = ''
    mkdir -p test-home
    export HOME=$PWD/test-home
  '';

  meta = with lib; {
    description = "Self-hosted pastebin powered by Git, open-source alternative to Github Gist.";
    homepage = "https://github.com/thomiceli/opengist";
    license = licenses.agpl3;
    mainProgram = "opengist";
  };
}
