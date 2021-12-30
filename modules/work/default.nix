{ pkgs, ... }:

{
  home.sessionVariables = {
    SBT_CREDENTIALS = "$HOME/.sbt/banno-credentials.properties";
  };
}
