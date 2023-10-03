let
  module = ./module.nix;
  modules = {
    garnix = module;
    default.imports = module;
  };
in
{
  flake = {
    darwinModules = modules;
    nixosModules = modules;
    homeModules = modules;
  };
}
