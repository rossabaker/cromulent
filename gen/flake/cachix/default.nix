{
  flake = {
    nixosModules.cachix = ../../modules/cachix;
    darwinModules.cachix = ../../modules/cachix;
    homeModules.cachix = ../../modules/cachix;
  };
}
