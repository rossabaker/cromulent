{
  flake = {
    nixosModules.binaryCaches = ../../modules/binaryCaches;
    darwinModules.binaryCaches = ../../modules/binaryCaches;
    homeModules.binaryCaches = ../../modules/binaryCaches;
  };
}
