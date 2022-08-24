{ src, emacsNativeComp, hugo, stdenv }:

let
  siteEmacs = emacsNativeComp.pkgs.withPackages (epkgs: [
    epkgs.ox-hugo
  ]);
in
stdenv.mkDerivation rec {
  name = "rossabaker.com";
  inherit src;
  buildInputs = [ siteEmacs hugo ];
  buildPhase = ''
    cd ..
    ${siteEmacs}/bin/emacs -Q --batch --script ${./export.el}
    ${hugo}/bin/hugo --config .hugo-out/config.toml
  '';
  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
