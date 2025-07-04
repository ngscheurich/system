{ ... }:

{
  homebrew = {
    enable = true;

    brewPrefix = "/opt/homebrew/bin";

    onActivation.cleanup = "zap";

    global.brewfile = true;

    taps = [
      "charmbracelet/tap"
      "d12frosted/emacs-plus"
      "homebrew/cask-versions"
      "homebrew/services"
      "morantron/homebrew-tmux-fingers"
      "nikitabobko/tap"
    ];

    brews = [
      "asyncapi"
      "colima"
      "docker"
      "docker-compose"
      "docker-credential-helper"
      "llvm"
      "luajit"
      "mods"
      "pngpaste"

      {
        name = "emacs-plus";
        args = [
          "with-xwidgets"
          "with-savchenkovaleriy-big-sur-icon"
        ];
      }

      # tmux-fingers + deps ------------
      "tmux-fingers"
      "bdw-gc"
      "libevent"
      "libyaml"
      "pcre2"
      # --------------------------------
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
      "dropbox"
      "fantastical"
      "figma"
      "firefox"
      "ghostty"
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
      "obsidian"
      "opal-composer"
      "postico"
      "postman"
      "raycast"
      "reaper"
      "vlc"
    ];

    masApps = {
      Slack = 803453959;
      Xcode = 497799835;
    };
  };
}
