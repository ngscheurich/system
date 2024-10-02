{ ... }:

{
  homebrew = {
    enable = true;

    brewPrefix = "/opt/homebrew/bin";

    onActivation.cleanup = "zap";

    global.brewfile = true;

    taps = [
      "gcenx/wine"
      "homebrew/cask-versions"
      "homebrew/services"
      "morantron/homebrew-tmux-fingers"
      "nikitabobko/tap"
      "railwaycat/emacsmacport"
    ];

    brews = [
      "asyncapi"
      "llvm"
      "luajit"
      "tmux-fingers"

      # tmux-fingers dependencies
      "bdw-gc"
      "libevent"
      "libyaml"
      "pcre2"
    ];

    casks = [
      "1password"
      "1password-cli"
      "appcleaner"
      "aerospace"
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
      "logseq"
      "magicavoxel"
      "obs"
      "opal-composer"
      "postico"
      "postman"
      "raycast"
      "reaper"
      "todoist"
      "vlc"
      "whatsapp"
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
