{ pkgs, home, config, ... }:
let
  cfg = config.programs.emacs;
in
{
  xdg.configFile."emacs/load-path.el".source = pkgs.writeText "load-path.el" ''
    (let ((default-directory (file-name-as-directory
			      "${cfg.package.deps}/share/emacs/site-lisp/"))
	  (normal-top-level-add-subdirs-inode-list nil))
    (normal-top-level-add-subdirs-to-load-path))
  '';
}
