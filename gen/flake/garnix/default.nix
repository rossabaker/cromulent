{
  flake = {
    darwinModules.garnix = ./module.nix;
    nixosModules.garnix = ./module.nix;
    homeModules.garnix = ./module.nix;
  };
}
