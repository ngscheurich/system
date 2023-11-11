{ ... }:

{
  homebrew = {
    enable = true;

    brewPrefix = "/opt/homebrew/bin";

    onActivation.cleanup = "zap";

    global.brewfile = true;

    taps = [
      "homebrew/cask"
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

      # Emacs dependencies
      "gmp"
      "libunistring"
      "gettext"
      "libidn2"
      "libtasn1"
      "nettle"
      "p11-kit"
      "openssl@1.1"
      "libevent"
      "libnghttp2"
      "unbound"
      "gnutls"
      "libpng"
      "freetype"
      "fontconfig"
      "pcre2"
      "glib"
      "xorgproto"
      "libxau"
      "libxdmcp"
      "libxcb"
      "libx11"
      "libxext"
      "libxrender"
      "lzo"
      "pixman"
      "cairo"
      "jpeg-turbo"
      "libtiff"
      "gdk-pixbuf"
      "fribidi"
      "graphite2"
      "icu4c"
      "harfbuzz"
      "pango"
      "librsvg"
      "little-cms2"
      "jansson"
      "tree-sitter"
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
