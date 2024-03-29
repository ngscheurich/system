{ ... }:

{
  homebrew = {
    enable = true;

    brewPrefix = "/opt/homebrew/bin";

    onActivation.cleanup = "zap";

    global.brewfile = true;

    taps = [
      "homebrew/cask-versions"
      "homebrew/services"
      "railwaycat/emacsmacport"
    ];

    brews = [
      "asyncapi"
      "llvm"
    ];

    casks = [
      "1password"
      "1password-cli"
      "amethyst"
      "appcleaner"
      "bartender"
      "cleanshot"
      "daisydisk"
      "dash"
      "discord"
      "docker"
      "dropbox"
      "emacs-mac"
      "fantastical"
      "figma"
      "firefox"
      "gpg-suite-no-mail"
      "hammerspoon"
      "istat-menus"
      "karabiner-elements"
      "keycastr"
      "kitty"
      "livebook"
      "logitech-options"
      "magicavoxel"
      "obs"
      "obsidian"
      "postico"
      "postman"
      "raycast"
      "reaper"
      "todoist"
      "vlc"
      {
        name = "gcenx/wine/wineskin";
        args = { no_quarantine = true; };
      }
      "wireshark"
    ];

    masApps = {
      Slack = 803453959;
      Things = 904280696;
      Xcode = 497799835;
    };
  };
}
