{ pkgs, ... }:

{
  system.stateVersion = "21.11";

  boot.loader.grub = {
    enable = true;
    version = 2;
  };

  networking.hostName = "twoflower";

  time.timeZone = "US/Central";

  networking = {
    usePredictableInterfaceNames = false;
    useDHCP = false;
    interfaces.eth0.useDHCP = true;
  };

  users.users.nick = {
    isNormalUser = true;
    home = "/home/nick";
    description = "N. G. Scheurich";
    shell = pkgs.fish;
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCaTulIsVGH+dR36P2N2TP6SwlX6hyBPW7t4GbZlXg4WVkAk2w4TWUILfMPswK3KbGf3YkuvtLXJpagwaxCFHGWq80wGL+O78zx33zE2meUAzXgnGf6K3x+T8w+TWYYp9kNx+3gXDtkc6kCQZ8GksYK+tsOCV40jujqFuXKA4WTuAfSvWpdGWwPrlH89XBIVRo/3zQE2V/uC5ySccmGI9i4PP/e5lVifvqqHimnbnXnLhsB3w4WtfyEnERACnhwPTJ33pviWx16/JzQCdpfliw0fY8O8ps5fzVUesH8pUewn7yKHfjYLsW5QSfK3XJaCpAsJsjkS6x4d7RNTo/tMuZH+eD9wEQOSp7vXv292GogoObWlDccjRmHJ2rDO37ewVTDZZjZ+8SV0LB/uiLIL4k2a0ONAINuiZslUouoMiMXLXQKdqDJswWsaBHQyuGT10N6YfOb2ToTZgmrTsZhUmUOwPp6dRfPiisk+5stXvAI8fzpsvRd87X7cUAKWKI0= nick@ridcully" ];
  };

  environment = {
    systemPackages = with pkgs; [
      inetutils
      mtr
      sysstat
      vim
    ];

    shells = [ pkgs.fish ];
  };

  nixpkgs.config.allowUnfree = true;

  programs.gnupg.agent = {
    enable = true;
  };

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
  };
}
