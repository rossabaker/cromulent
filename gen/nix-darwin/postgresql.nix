{ config, pkgs, ... }:

let
  dataDir = "/var/lib/postgresql/14";
  logDir = "/var/log/postgresql";
  user = "RABaker";
in
{
  services = {
    postgresql = {
      inherit dataDir;
      enable = true;
      package = pkgs.postgresql_14;
      initdbArgs = [ "-D" dataDir ];
      authentication =
	''
        # On first run, use "trust" instead of "scram-sha-256"
	local all postgres scram-sha-256
	'';
    };
  }
;
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
