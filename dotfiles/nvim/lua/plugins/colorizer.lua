return {
  "NvChad/nvim-colorizer.lua",

  event = "BufReadPre",

  config = {
    user_default_options = {
      AARRGGBB = true,
      RGB = true,
      RRGGBB = true,
      RRGGBBAA = true,
      hsl_fn = true,
      names = false,
      rgb_fn = true,
    },
  },
}
