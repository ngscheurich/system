{ pkgs, inputs, ... }:

{
  system.defaults = {
    dock = {
      autohide = true;
      autohide-delay = "0.0";
      autohide-time-modifier = "1.0";
      mineffect = "scale";
      show-recents = false;
      tilesize = 48;
      wvous-br-corner = 1;
    };

    finder = {
      AppleShowAllFiles = true;
      AppleShowAllExtensions = true;
    };

    NSGlobalDomain = {
      AppleInterfaceStyle = "Dark";
      AppleKeyboardUIMode = 3;
      ApplePressAndHoldEnabled = false;

      InitialKeyRepeat = 15;
      KeyRepeat = 2;

      NSAutomaticSpellingCorrectionEnabled = false;
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
    };

    loginwindow = {
      GuestEnabled = false;
      SHOWFULLNAME = false;
    };

    screencapture.location = "~/Screenshots";
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };
}
