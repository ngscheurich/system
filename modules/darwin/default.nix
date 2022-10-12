{ pkgs, inputs, ... }:

{
  imports = [
    ./homebrew.nix
    ./preferences.nix
  ];

  system.stateVersion = 4;

  services.nix-daemon.enable = true;

  nix = {
    gc = {
      automatic = true;
      interval = { Hour = 3; Minute = 15; };
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = inputs.overlays;
  };

  programs.fish.enable = true;
  programs.zsh.enable = true;

  environment.shells = [ pkgs.fish pkgs.zsh ];
}
