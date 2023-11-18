{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./mastodon.nix
    ];

  # Use GRUB2 as the boot loader.
  # We don't use systemd-boot because Hetzner uses BIOS legacy boot.
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    enable = true;
    efiSupport = false;
    devices = [ "/dev/sda" "/dev/sdb" ];
  };

  networking.hostName = "abe";

  # The mdadm RAID1s were created with 'mdadm --create ... --homehost=hetzner',
  # but the hostname for each machine may be different, and mdadm's HOMEHOST
  # setting defaults to '<system>' (using the system hostname).
  # This results mdadm considering such disks as "foreign" as opposed to
  # "local", and showing them as e.g. '/dev/md/hetzner:root0'
  # instead of '/dev/md/root0'.
  # This is mdadm's protection against accidentally putting a RAID disk
  # into the wrong machine and corrupting data by accidental sync, see
  # https://bugzilla.redhat.com/show_bug.cgi?id=606481#c14 and onward.
  # We do not worry about plugging disks into the wrong machine because
  # we will never exchange disks between machines, so we tell mdadm to
  # ignore the homehost entirely.
  environment.etc."mdadm.conf".text = ''
    HOMEHOST <ignore>
  '';
  # The RAIDs are assembled in stage1, so we need to make the config
  # available there.
  boot.swraid.mdadmConf = ''
    HOMEHOST <ignore>
  '';

  # Network (Hetzner uses static IP assignments, and we don't use DHCP here)
  networking.useDHCP = false;
  networking.interfaces."enp27s0".ipv4.addresses = [
    {
      address = "95.216.32.37";
      # FIXME: The prefix length is commonly, but not always, 24.
      # You should check what the prefix length is for your server
      # by inspecting the netmask in the "IPs" tab of the Hetzner UI.
      # For example, a netmask of 255.255.255.0 means prefix length 24
      # (24 leading 1s), and 255.255.255.192 means prefix length 26
      # (26 leading 1s).
      prefixLength = 26;
    }
  ];
  networking.interfaces."enp27s0".ipv6.addresses = [
    {
      address = "2a01:4f9:2a:2047::1";
      prefixLength = 64;
    }
  ];
  networking.defaultGateway = "95.216.32.1";
  networking.defaultGateway6 = { address = "fe80::1"; interface = "enp27s0"; };
  networking.nameservers = [ "8.8.8.8" ];

  # Initial empty root password for easy login:
  users.users.root.initialHashedPassword = "";
  services.openssh.settings.PermitRootLogin = "prohibit-password";

  users.users.root.openssh.authorizedKeys.keys = [
    # FIXME Replace this by your SSH pubkey!
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFTwvnkKWbNTAPSiK/agHOV9xXoh1R6hx5B8lZYmPY6Z"
  ];

  services.openssh.enable = true;

  # FIXME
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "23.05"; # Did you read the comment?

}
