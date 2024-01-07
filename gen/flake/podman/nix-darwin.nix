{
  flake.darwinModules.podman =
    { config, lib, pkgs, ... }:
    {
      environment.systemPackages =
        [
          pkgs.podman
          pkgs.qemu
          pkgs.xz
        ];

      # https://github.com/containers/podman/issues/17026
      environment.pathsToLink = [ "/share/qemu" ];

      # https://github.com/LnL7/nix-darwin/issues/432#issuecomment-1024951660
      environment.etc."containers/containers.conf.d/99-gvproxy-path.conf".text = ''
        [engine]
        helper_binaries_dir = ["${pkgs.gvproxy}/bin"]
      '';
    };
}
