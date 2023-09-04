{ src, emacs, gnupg, graphviz, hugo, html5validator, hyperlink, stdenv }:

stdenv.mkDerivation rec {
  name = "rossabaker.com";
  inherit src;
  nativeBuildInputs = [
    emacs
    gnupg
    graphviz
    hugo
    html5validator
    hyperlink
  ];
  buildPhase = ''
    cd ..
    export PATH=${gnupg}/bin:$PATH

    ${emacs}/bin/emacs -Q --batch -l ${./code-license-header.el} -l ${./export.el}

    # Reassemble netlify.toml from its constitutents
    for toml in tmp/netlify.toml.d/*; do
      cat $toml >> tmp/hugo/static/netlify.toml
    done

    ${hugo}/bin/hugo --config tmp/hugo/config.toml
  '';

  doCheck = true;
  checkPhase = ''
    echo YO WTF
    cat tmp/hugo/content/license.md
    html5validator --log INFO --root public
    hyperlink public/ --check-anchors
  '';

  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
