return {
  "folke/zen-mode.nvim",

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

  cmd = "ZenMode",
}
