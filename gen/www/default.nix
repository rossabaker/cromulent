{ src, emacsNativeComp, gnupg, hugo, stdenv }:
let
  siteEmacs = emacsNativeComp.pkgs.withPackages (epkgs: [
    epkgs.htmlize
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
    export PATH=${gnupg}/bin:${hugo}/bin:$PATH
    export HOME=$(pwd)
    ${siteEmacs}/bin/emacs -Q --batch \
      --script ${./build.el} \
      --eval "(ross-www/legacy-publish)" \
      --eval "(ross-www/publish)"
    # A horrific hack
    echo '/.well-known/webfinger resource=acct:ross@rossabaker.com https://indieweb.social/.well-known/webfinger?resource=acct%3Arossabaker%40indieweb.social 301' >> tmp/public_html/_redirects
  '';
  installPhase = ''
    mkdir $out
    cp -r tmp/public_html/. $out
  '';
}
