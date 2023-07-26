{ pkgs, inputs, ... }:

{
  imports = [
    ./homebrew.nix
    ./preferences.nix
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';

    gc = {
      automatic = true;
      interval = { Hour = 3; Minute = 15; };
    };
  };

  services.nix-daemon.enable = true;

  system.stateVersion = 4;

  nixpkgs = {
    config.allowUnfree = true;
  };

  programs.zsh.enable = true;
  programs.fish.enable = true;

  environment.shells = with pkgs; [ zsh fish ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
