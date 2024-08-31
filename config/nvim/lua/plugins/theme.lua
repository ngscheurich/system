-- ===================================================================
--  Colors and Iconography
-- ===================================================================

local util = require("util")

return {
  -- =================================================================
  --  nvim-base16
  -- -----------------------------------------------------------------
  --   https://github.com/RRethy/base16-nvim
  --   Plugin for building base16 colorschemes
  -- -----------------------------------------------------------------
  {
    "RRethy/base16-nvim",
    cond = vim.startswith(_G.theme.colorscheme.name, "base16"),
    lazy = false,
    priority = 1000,
    config = function()
      require("base16-colorscheme").with_config(_G.theme.colorscheme.opts)
      util.apply_colorscheme()
    end,
  },

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
