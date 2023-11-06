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

    ${emacs}/bin/emacs -Q --batch -l ${./export.el}

    # Reassemble netlify.toml from its constitutents
    for toml in tmp/netlify.toml.d/*; do
      cat $toml >> tmp/hugo/static/netlify.toml
    done

    ${hugo}/bin/hugo --config tmp/hugo/config.toml
  '';

  doCheck = true;
  checkPhase = ''
    # https://github.com/validator/validator/issues/1397
    # https://github.com/validator/validator/issues/1569
    html5validator --log INFO --root public \
      --ignore-re 'Element "search" not allowed' \
                  'Element "p" not allowed as child of element "hgroup"'
    hyperlink public/ --check-anchors
  '';

  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
