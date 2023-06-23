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

    # https://emacs.stackexchange.com/a/70847
    ${emacs}/bin/emacs --batch -l ob -l ob-shell --eval "
      (let ((org-confirm-babel-evaluate nil))
	(with-current-buffer (find-file-noselect \"src/org/configs/website.org\")
	  (org-babel-execute-buffer)
	  (save-buffer)))
    "

    # Reassemble netlify.toml from its constitutents
    for toml in tmp/netlify.toml.d/*; do
      cat $toml >> tmp/hugo/static/netlify.toml
    done

    ${hugo}/bin/hugo --config tmp/hugo/config.toml
  '';

  doCheck = true;
  checkPhase = ''
    html5validator --log INFO --root tmp/hugo/static
    hyperlink public/ --check-anchors
  '';

  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
