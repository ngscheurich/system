# ----------------------------------------------------------
# Packages for tracking tasks and time spent on them
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    taskwarrior
    taskwarrior-tui
    timewarrior
  ];
}
