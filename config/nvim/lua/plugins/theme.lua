-- ===================================================================
--  Colors and Iconography
-- ===================================================================

local util = require("util")

return {
  -- =================================================================
  --  Nightfox
  -- -----------------------------------------------------------------
  --  https://github.com/EdenEast/nightfox.nvim
  --  A highly-customizable theme for Vim and Neovim
  -- -----------------------------------------------------------------
  {
    "EdenEast/nightfox.nvim",
    cond = vim.endswith(_G.theme.colorscheme.name, "fox"),
    lazy = false,
    priority = 1000,
    config = function()
      require("nightfox").setup(_G.theme.colorscheme.opts)
      util.apply_colorscheme()
    end,
  },

  -- =================================================================
  --  Catppuccin
  -- -----------------------------------------------------------------
  --  https://github.com/catppuccin/nvim
  --  Soothing pastel theme for (Neo)vim
  -- -----------------------------------------------------------------
  {
    "catppuccin/nvim",
    name = "catppuccin",
    cond = vim.startswith(_G.theme.colorscheme.name, "catppuccin"),
    lazy = false,
    priority = 1000,
    config = function()
      require("catppuccin").setup(_G.theme.colorscheme.opts)
      util.apply_colorscheme()
    end,
  },

  -- =================================================================
  --  Tokyo Night
  -- -----------------------------------------------------------------
  --  https://github.com/folke/tokyonight.nvim
  --  Clean, dark theme with support lots of plugins
  -- -----------------------------------------------------------------
  {
    "folke/tokyonight.nvim",
    cond = vim.startswith(_G.theme.colorscheme.name, "tokyonight"),
    lazy = false,
    priority = 1000,
    config = function()
      require("tokyonight").setup(_G.theme.colorscheme.opts)
      vim.cmd.colorscheme("tokyonight")
    end,
  },

  -- =================================================================
  --  nvim-web-devicons
  -- -----------------------------------------------------------------
  --  https://github.com/nvim-tree/nvim-web-devicons
  --  Colored icons for filetypes
  -- -----------------------------------------------------------------
  { "nvim-tree/nvim-web-devicons", config = true },

  -- =================================================================
  --  nvim-nonicons
  -- -----------------------------------------------------------------
  --  https://github.com/yamatsum/nvim-nonicons
  --  Crisp, thin icon set for nvim-web-devicons
  -- -----------------------------------------------------------------
  { "yamatsum/nvim-nonicons", config = true },

  -- =================================================================
  --  Todo Comments
  -- -----------------------------------------------------------------
  --  https://github.com/folke/todo-comments.nvim
  --  Highlights and indexes TODO-style comments
  -- -----------------------------------------------------------------
  {
    "folke/todo-comments.nvim",
    event = "VimEnter",
    dependencies = { "nvim-lua/plenary.nvim", "folke/trouble.nvim" },
    config = true,
  },
}
