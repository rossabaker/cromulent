{ ... }:

{
  services.borgbackup.jobs.test = {
    paths = [ "/root/cromulent" ];
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/borgbackup/passphrase";
    };
    repo = "ssh://u377329@u377329.your-storagebox.de:23/home/backups/abe";
    doInit = true;
    compression = "auto,zstd";
    startAt = "daily";
  };
}
