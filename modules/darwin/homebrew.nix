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
      "trash"
      "wxwidgets"
    ];

    casks = [
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
      "spotify"
      "statusfy"
      "todoist"
      "wireshark"
    ];

    masApps = {
      Slack = 803453959;
    };
  };
}
