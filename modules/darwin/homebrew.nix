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
      "koekeishiya/formulae"
      "FelixKratz/formulae"
    ];

    brews = [
      "asyncapi"
      "koekeishiya/formulae/yabai"
      "koekeishiya/formulae/skhd"
      "llvm"
      "luajit"
    ];

    casks = [
      "1password"
      "1password-cli"
      "appcleaner"
      "amethyst"
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
      Xcode = 497799835;
    };
  };
}
