{ ... }:

{
  services.borgbackup.jobs.test = {
    paths = [ "/root/cromulent" ];
    encryption.mode = "none";
    repo = "ssh://u377329@u377329.your-storagebox.de:/home/backups/abe";
    compression = "auto,zstd";
    startAt = "daily";
  };
}
