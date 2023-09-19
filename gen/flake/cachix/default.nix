{ ... }:
{
  flake.darwinModules.cachix =
    { ... }:
    {
      nix.settings = {
        substituters = [
          "https://nix-community.cachix.org/"
          "https://rossabaker.cachix.org/"
          "https://typelevel.cachix.org/"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "rossabaker.cachix.org-1:KK/CQTeAGEurCUBy3nDl9PdR+xX+xtWQ0C/GpNN6kuw="
          "typelevel.cachix.org-1:UnD9fMAIpeWfeil1V/xWUZa2g758ZHk8DvGCd/keAkg="
        ];
      };
    };
}
