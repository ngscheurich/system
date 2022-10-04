{ pkgs, inputs, ... }:

{
  system.stateVersion = 4;

  services.nix-daemon.enable = true;

  nix = {
    gc = {
      automatic = true;
      interval = { Hour = 3; Minute = 15; };
    };
    extraOptions = "experimental-features = nix-command flakes";
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = inputs.overlays;
  };

  environment.shells = [
    pkgs.fish
  ];

  programs.fish.enable = true;
}
