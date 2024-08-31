# ----------------------------------------------------------
# Packages for tracking tasks and time spent on them
# ----------------------------------------------------------

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    taskwarrior3
    taskwarrior-tui
    timewarrior
  ];
}
