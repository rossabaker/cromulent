{ src, emacs, gnupg, hugo, html5validator, hyperlink, stdenv }:

stdenv.mkDerivation rec {
  name = "rossabaker.com";
  inherit src;
  nativeBuildInputs = [
    emacs
    gnupg
    hugo
    html5validator
    hyperlink
  ];
  buildPhase = ''
    cd ..
    export PATH=${gnupg}/bin:$PATH

    ${emacs}/bin/emacs -Q --batch --script ${./export.el}

    # Reassemble netlify.toml from its constitutents
    for toml in tmp/netlify.toml.d/*; do
      cat $toml >> tmp/hugo/static/netlify.toml
    done

    ${hugo}/bin/hugo --config tmp/hugo/config.toml
  '';

  doCheck = true;
  checkPhase = ''
    html5validator --log INFO --root public
    hyperlink public/ --check-anchors
  '';

  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
