{ config, ... }:

{
  age.secrets.abe-backups-default.file = ../../../secrets/abe-backups-default.age;

  services.borgbackup.jobs.default = {
    paths = [
      "/etc"
      "/home"
      "/var"
      "/root"
    ];
    exclude = [
      "/var/cache"
      "/var/tmp"
    ];
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat ${config.age.secrets.abe-backups-default.path}";
    };
    repo = "ssh://u377329@u377329.your-storagebox.de:23/home/backups/abe/default";
    doInit = true;
    compression = "auto,zstd";
    startAt = "daily";
  };
}
