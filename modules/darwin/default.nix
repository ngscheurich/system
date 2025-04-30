{ pkgs, ... }:

{
  imports = [
    ./homebrew.nix
    ./preferences.nix
  ];

  nix = {
    enable = true;

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

  system.stateVersion = 4;

  nixpkgs = {
    config.allowUnfree = true;
  };

  programs.zsh.enable = true;
  programs.fish.enable = true;

  environment.shells = with pkgs; [ zsh fish ];
  environment.pathsToLink = [ "/share/zsh" ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
