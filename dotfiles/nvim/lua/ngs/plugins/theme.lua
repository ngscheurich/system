return {
  -- Icons
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons", config = true },

  -- Colorschemes
  {
    "folke/tokyonight.nvim",
    cond = vim.g.theme == "tokyonight",
    config = function()
      require("tokyonight").setup({
        style = "night",
        sidebars = { "qf", "terminal", "help" },
      })
      vim.cmd.colorscheme("tokyonight")
    end,
  },
  {
    "miikanissi/modus-themes.nvim",
    cond = vim.g.theme == "modus",
    config = function()
      require("modus-themes").setup({
        style = "auto",
        variant = "tinted",
        styles = {},
      })
      vim.cmd.colorscheme("modus")
    end,
  },
}
