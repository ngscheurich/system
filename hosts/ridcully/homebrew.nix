{ ... }:

{
  homebrew.enable = true;

  homebrew = {
    brewPrefix = "/opt/homebrew/bin";
    cleanup = "zap";
    global.brewfile = true;
    global.noLock = true;
  };

  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-drivers"
    "homebrew/cask-versions"
    "homebrew/core"
    "homebrew/services"
    "koekeishiya/formulae"
  ];

  homebrew.brews = [
    "asdf"
    "lunchy"
    "postgresql@14"
    "shpotify"
    "trash"
    "wxwidgets"
  ];

  homebrew.casks = [
    "1password"
    "1password-cli"
    "amethyst"
    "appcleaner"
    "bartender"
    "daisydisk"
    "dash"
    "dropbox"
    "fantastical"
    "figma"
    "firefox"
    "google-drive"
    "gpg-suite-no-mail"
    "hammerspoon"
    "istat-menus"
    "karabiner-elements"
    "keycastr"
    "kitty"
    "logitech-options"
    "logseq"
    "postico"
    "postman"
    "raycast"
    "slack"
    "spotify"
    "statusfy"
    "todoist"
    "wireshark"
  ];

  homebrew.masApps = {};
}
