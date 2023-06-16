{ inputs, ... }:

{
  default = final: prev:
    import ../pkgs {
      inherit inputs;
      pkgs = final;
    };
}
