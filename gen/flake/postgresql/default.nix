let
  dataDir = "/var/lib/postgresql/14";
  logDir = "/var/log/postgresql";
  user = "RABaker";
  port = 5433;
in
{
  flake.darwinModules.postgresql = (
    { config, pkgs, ... }:
    {
      services = {
        postgresql = {
          inherit dataDir port;
          enable = true;
          package = pkgs.postgresql_14;
          initdbArgs = [ "-D" dataDir ];
        };
      };

      users = {
        knownUsers = [ "postgres" ];
        users = {
          postgres = {
            uid = 2000;
            shell = "/bin/bash";
          };
        };
      };

      # Create the PostgreSQL data directory, if it does not exist.
      system.activationScripts.preActivation = {
        enable = true;
        text = ''
        if [ ! -d "${dataDir}" ]; then
          echo "creating PostgreSQL data directory..."
          sudo mkdir -m 700 -p "${dataDir}/"
          chown -R ${user}:staff "${dataDir}/"
        fi

        if [ ! -d "/var/log/postgresql" ]; then
          echo "creating PostgreSQL log directory..."
          sudo mkdir -m 700 -p "${logDir}/"
          chown -R ${user}:staff "${logDir}/"
        fi
      '';
      };

      launchd.user.agents.postgresql.serviceConfig = {
        StandardErrorPath = "${logDir}/postgres.error.log";
        StandardOutPath = "${logDir}/postgres.out.log";
      };
    }
  );
}
