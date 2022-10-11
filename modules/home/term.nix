# ----------------------------------------------------------
# The kitty termminal emulator, plus configuration
# ----------------------------------------------------------

{ ... }:

{
  programs.kitty = {
    enable = true;

    font = {
      # name = "PragmataPro Mono Liga";
      # name = "FiraCode-Retina";
      name = "MonoLisa";
      size = 12;
    };

    settings = {
      adjust_line_height = "110%";
      allow_remote_control = "yes";
      copy_on_select = "yes";
      enable_audio_bell = "no";
      font_features = "FiraCode-Retina +cv14 +ss01 +ss08";
      macos_option_as_alt = "no";
      macos_thicken_font = "0.1";
      strip_trailing_spaces = "smart";
      term = "xterm-256color";
      visual_bell_duration = 0;
      window_padding_width = 4;
    };

    extraConfig = ''
      include ./colors.conf
    '';

    darwinLaunchOptions = [
      "--listen-on=unix:/tmp/kittysocket"
    ];
  };
}
