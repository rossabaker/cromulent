{ withSystem, ... }:
{ config, lib, pkgs, ... }:

let
  cfg = config.cromulent.services.opengist;

  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    types
    ;
in
{
  options.cromulent.services.opengist = {
    enabled = mkEnableOption "Opengist" // {
      default = true;
    };

    package = mkOption {
      type = types.package;
      description = "The opengist package to use";
      default = withSystem pkgs.stdenv.hostPlatform.system ({ config, ... }:
        config.packages.opengist
      );
      defaultText = "`packages.opengist` from the Cromulent flake";
    };

    domain = mkOption {
      type = types.str;
      description = "Domain name of Opengist server";
      example = "paste.example.com";
    };

    user = mkOption {
      type = types.str;
      default = "opengist";
      description = "User account under which Opengist runs";
    };

    group = mkOption {
      type = types.str;
      default = "opengist";
      description = "Group under which Opengist runs";
    };

    configureNginx = mkOption {
      type = types.bool;
      default = true;
      description = "Configures Nginx as a reverse proxy for Opengist.";
    };
  };

  config = mkIf cfg.enabled {
    services.nginx = lib.mkIf cfg.configureNginx {
      enable = true;
      virtualHosts."${cfg.domain}" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:6157";
        };
      };
    };

    systemd.services.opengist = {
      description = "Opengist paste bin, powered by Git";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ cfg.package pkgs.git pkgs.openssh ];
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = lib.getExe cfg.package;
        WorkingDirectory = cfg.package;
        StateDirectory = "opengist";
        StateDirectoryMode = "0750";
        LogsDirectory = "opengist";
        LogsDirectoryMode = "0750";
      };
      environment = {
        OG_OPENGIST_HOME = "/var/lib/opengist";
      };
    };

    users.users = mkIf (cfg.user == "opengist") {
      opengist = {
        isSystemUser = true;
        home = "/var/lib/opengist";
        inherit (cfg) group;
      };
    };

    users.groups = mkIf (cfg.group == "opengist") {
      opengist = { };
    };
  };
}
