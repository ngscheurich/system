{ pkgs, inputs, ... }:

{
  system.defaults = {
    dock = {
      autohide = true;
      mineffect = "scale";
      show-recents = false;
      tilesize = 48;
      wvous-br-corner = 1;
    };

    finder = {
      AppleShowAllFiles = true;
    };

    NSGlobalDomain = {
      AppleInterfaceStyle = "Dark";
      AppleKeyboardUIMode = 3;
      InitialKeyRepeat = 15;
      KeyRepeat = 2;
      NSAutomaticSpellingCorrectionEnabled = false;
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
    };

    loginwindow.GuestEnabled = false;
    screencapture.location = "~/Screenshots";
  };

}
