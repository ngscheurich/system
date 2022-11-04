{ pkgs, ... }:

{
  imports = [
    ./configuration.nix
    ./hardware.nix
    ./home.nix
  ];
}
