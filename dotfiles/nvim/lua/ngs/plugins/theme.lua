return {
  -- Icons
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons", config = true },

  -- Colorschemes
  {
    "folke/tokyonight.nvim",
    cond = vim.g.colorscheme == "tokyonight",
    config = function()
      require("tokyonight").setup({
        style = "night",
        sidebars = { "qf", "terminal", "help" },
      })
      vim.cmd.colorscheme("tokyonight")
    end,
  },
}
