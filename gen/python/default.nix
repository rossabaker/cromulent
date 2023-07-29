{ moduleWithSystem, ... }: {
  flake.homeManagerModules.python = moduleWithSystem(
    perSystem@{ pkgs }: {
      home.file = {
	".config/zsh/conda-setup".source = ./conda-setup;
      };

      programs.zsh.initExtra = ''
      . ~/.config/zsh/conda-setup
      '';
    }
  );
}
