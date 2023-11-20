{ ... }:

{
  users.users.ross = {
    isNormalUser = true;
    description = "Ross A. Baker";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFTwvnkKWbNTAPSiK/agHOV9xXoh1R6hx5B8lZYmPY6Z"
    ];
  };
}
