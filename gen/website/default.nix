{ src, emacs29, gnupg, hugo, stdenv }:

let
  siteEmacs = emacs29.pkgs.withPackages (epkgs: [
    epkgs.ox-hugo
  ]);
in
stdenv.mkDerivation rec {
  name = "rossabaker.com";
  inherit src;
  nativeBuildInputs = [
    siteEmacs
    gnupg
    hugo
  ];
  buildPhase = ''
    cd ..
    export PATH=${gnupg}/bin:$PATH
    ${siteEmacs}/bin/emacs -Q --batch --script ${./export.el}
    ${hugo}/bin/hugo --config tmp/hugo/config.toml
  '';
  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
