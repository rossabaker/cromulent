{ src, emacs29, gnupg, hugo, html5validator, hyperlink, stdenv }:

let
  siteEmacs = emacs29.pkgs.withPackages (epkgs: [
    epkgs.dash
    epkgs.esxml
    epkgs.git-link
    epkgs.htmlize
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
    html5validator
    hyperlink
  ];
  buildPhase = ''
    cd ..
    export PATH=${gnupg}/bin:$PATH

    # https://emacs.stackexchange.com/a/70847
    ${siteEmacs}/bin/emacs --batch -l ob -l ob-shell --eval "
      (let ((org-confirm-babel-evaluate nil))
	(with-current-buffer (find-file-noselect \"src/org/configs/website.org\")
	  (org-babel-execute-buffer)
	  (save-buffer)))
    "
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
