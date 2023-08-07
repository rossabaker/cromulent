{ config, lib, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      pkgs.podman
      pkgs.qemu
      pkgs.xz
    ];

  environment.pathsToLink = [ "/share/qemu" ];

  programs.zsh.interactiveShellInit = lib.strings.concatStringsSep "\n" [
    config.system.build.setAliases.text
    "export DOCKER_HOST=unix://$(podman machine inspect --format '{{.ConnectionInfo.PodmanSocket.Path}}')"
  ];

  # https://github.com/LnL7/nix-darwin/issues/432#issuecomment-1024951660
  environment.etc."containers/containers.conf.d/99-gvproxy-path.conf".text = ''
    [engine]
    helper_binaries_dir = ["${pkgs.gvproxy}/bin"]
  '';
}
