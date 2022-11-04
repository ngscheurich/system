{ pkgs, ... }:

{
  imports = [
    ./configuration.nix
    ./home.nix
  ];
}
