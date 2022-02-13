# When you add custom packages, list them here
{ pkgs }: {
  website = pkgs.callPackage ./website { };
}
