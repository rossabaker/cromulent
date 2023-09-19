{ ... }:
{
  flake.darwinModules.keyboard =
    { ... }:
    {
      system.keyboard = {
        enableKeyMapping = true;
        remapCapsLockToControl = true;
      };
    };
}
