{ src, emacsNativeComp, gnupg, hugo, stdenv }:

let
  siteEmacs = emacsNativeComp.pkgs.withPackages (epkgs: [
    epkgs.ox-hugo
  ]);
in
stdenv.mkDerivation rec {
  name = "rossabaker.com";
  inherit src;
  buildInputs = [
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
