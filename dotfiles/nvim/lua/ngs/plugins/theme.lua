return {
  -- Colorschemes
  {
    "folke/tokyonight.nvim",
    cond = vim.g.theme == "tokyonight",
    lazy = false,
    priority = 1000,
    config = function()
      require("tokyonight").setup({
        style = "night",
        sidebars = { "qf", "terminal", "help" },
      })
      vim.cmd.colorscheme("tokyonight")
    end,
  },

  -- Icons
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons", config = true },

  -- Highlight TODO-style comments
  {
    "folke/todo-comments.nvim",
    event = "VimEnter",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },
}
