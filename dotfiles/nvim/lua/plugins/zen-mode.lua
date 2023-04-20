return {
  "folke/zen-mode.nvim",

  cmd = "ZenMode",

  config = {
    window = {
      options = {
        signcolumn = "no",
        number = false,
        relativenumber = false,
      },
    },
    plugins = {
      kitty = {
        enabled = true,
        font = "+2",
      },
    },
  },
}
