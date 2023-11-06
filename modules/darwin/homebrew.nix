{ ... }:

{
  homebrew = {
    enable = true;

    brewPrefix = "/opt/homebrew/bin";

    onActivation.cleanup = "uninstall";

    global.brewfile = true;

    taps = [
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-versions"
      "homebrew/core"
      "homebrew/services"
      "d12frosted/emacs-plus"
    ];

    brews = [
      {
        name = "emacs-plus@29";
        args = [ "with-xwidgets" "with-savchenkovaleriy-big-sur-icon" ];
      }
      "llvm"
      "zellij"
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
      "figma"
      "firefox"
      "gpg-suite-no-mail"
      "hammerspoon"
      "istat-menus"
      "karabiner-elements"
      "keycastr"
      "kitty"
      "logitech-options"
      "logseq"
      "obs"
      "postico"
      "postman"
      "raycast"
      "reaper"
      "tidal"
      "todoist"
      "vlc"
      "wireshark"
    ];

    masApps = {
      Slack = 803453959;
      Xcode = 497799835;
    };
  };
}
