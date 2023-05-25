{ src, emacs29, gnupg, hugo, stdenv }:

let
  siteEmacs = emacs29.pkgs.withPackages (epkgs: [
    epkgs.esxml
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
    # https://emacs.stackexchange.com/a/70847
    ${siteEmacs}/bin/emacs --batch -l ob -l ob-shell --eval "
      (let ((org-confirm-babel-evaluate nil))
	(with-current-buffer (find-file-noselect \"src/org/configs/website/index.org\")
	  (org-babel-execute-buffer)
	  (save-buffer)))
    "
    ${hugo}/bin/hugo --config tmp/hugo/config.toml
  '';
  installPhase = ''
    mkdir $out
    cp -r public/. $out
  '';
}
