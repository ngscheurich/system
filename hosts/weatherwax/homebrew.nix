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
    "cocoapods"
    "koekeishiya/formulae/yabai"
    "lunchy"
    "postgresql@14"
    "shpotify"
    "skhd"
    "trash"
    "wxwidgets"
  ];

  homebrew.casks = [
    "1password"
    "1password-cli"
    "ableton-live-lite"
    "affinity-designer"
    "affinity-photo"
    "affinity-publisher"
    "android-sdk"
    "android-studio"
    "appcleaner"
    "aptible"
    "balenaetcher"
    "bartender"
    "blender"
    "cleanshot"
    "daisydisk"
    "dash"
    "discord"
    "docker"
    "dropbox"
    "epic-games"
    "fantastical"
    "figma"
    "firefox"
    "flipper"
    "godot"
    "google-chrome"
    "google-drive"
    "gpg-suite-no-mail"
    "hammerspoon"
    "istat-menus"
    "itch"
    "karabiner-elements"
    "keycastr"
    "kitty"
    "logitech-options"
    "logseq"
    "love"
    "obsidian"
    "postico"
    "postman"
    "raycast"
    "reaper"
    "slack"
    "spotify"
    "statusfy"
    "todoist"
    "transmission"
    "unity-hub"
    "wireshark"
    "zoom"
    "zulu11"
  ];

  homebrew.masApps = {
    Capo = 696977615;
    GarageBand = 682658836;
    Harvest = 506189836;
    Numbers = 409203825;
    Pages = 409201541;
    TestFlight = 899247664;
    Xcode = 497799835;
  };
}
