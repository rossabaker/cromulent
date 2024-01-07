{ src, emacs, gawk, gnupg, graphviz, hugo, html5validator, hyperlink, stdenv }:

stdenv.mkDerivation rec {
  name = "rossabaker.com";
  inherit src;
  nativeBuildInputs = [
    emacs
    gawk
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

    # Make nginx-compatible redirects map
    ${gawk}/bin/awk 'NF { $1 = "~^"$1"/?$"; $2 = $2";"; print}' public/_redirects > public/_redirects.map
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
