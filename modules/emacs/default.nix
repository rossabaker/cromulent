{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs;
    };
    extraPackages = epkgs: [ epkgs.use-package ];      
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
}