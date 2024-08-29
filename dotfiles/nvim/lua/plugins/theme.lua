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
  --  Rosé Pine
  -- -----------------------------------------------------------------
  --   https://github.com/rose-pine/neovim
  --   Colorscheme for the classy minimalist
  -- -----------------------------------------------------------------
  {
    "rose-pine/neovim",
    as = "rose-pine",
    cond = vim.startswith(_G.theme.colorscheme.name, "rose-pine"),
    lazy = false,
    priority = 1000,
    config = function()
      require("rose-pine").setup(_G.theme.colorscheme.opts)
      util.apply_colorscheme()
    end,
  },

  -- =================================================================
  --  Solarized
  -- -----------------------------------------------------------------
  --   https://github.com/maxmx03/solarized.nvim
  --   Precision colors for machines and people
  -- -----------------------------------------------------------------
  {
    "maxmx03/solarized.nvim",
    cond = _G.theme.colorscheme.name == "solarized",
    lazy = false,
    priority = 1000,
    config = function()
      require("solarized").setup(_G.theme.colorscheme.opts)
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
  --  Kanagawa
  -- -----------------------------------------------------------------
  --  https://github.com/rebelot/kanagawa.nvim
  --  Colorscheme inspired by "The Great Wave off Kanagawa"
  -- -----------------------------------------------------------------
  {
    "rebelot/kanagawa.nvim",
    cond = vim.startswith(_G.theme.colorscheme.name, "kanagawa"),
    lazy = false,
    priority = 1000,
    config = function()
      require("kanagawa").setup(_G.theme.colorscheme.opts)
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
