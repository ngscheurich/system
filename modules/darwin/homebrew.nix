{ ... }:

{
  homebrew = {
    enable = true;

    brewPrefix = "/opt/homebrew/bin";

    global.brewfile = true;

    taps = [
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
      "railwaycat/emacsmacport"
    ];

    brews = [
      "emacs-mac"
      "fsouza/prettierd/prettierd"
      "lunchy"
      "shpotify"
      "wxwidgets"
    ];

    casks = [
      "1password"
      "1password-cli"
      "affinity-designer"
      "affinity-photo"
      "affinity-publisher"
      "amethyst"
      "appcleaner"
      "bartender"
      "cleanshot"
      "daisydisk"
      "dash"
      "discord"
      "docker"
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
      "obs"
      "obsidian"
      "postico"
      "postman"
      # "protonvpn"
      "raycast"
      "spotify"
      "statusfy"
      "todoist"
      "vlc"
      "wireshark"
    ];

    masApps = {
      Slack = 803453959;
    };
  };
}
