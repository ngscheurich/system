-- ===================================================================
--  Colors and Iconography
-- ===================================================================

local util = require("util")

return {
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
  --  Nightfox
  -- -----------------------------------------------------------------
  --  https://github.com/EdenEast/nightfox.nvim
  --  A highly-customizable theme for Vim and Neovim
  -- -----------------------------------------------------------------
  {
    "EdenEast/nightfox.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      if vim.endswith(_G.theme.colorscheme.name, "fox") then
        require("nightfox").setup(_G.theme.colorscheme.opts)
        util.apply_colorscheme()
      end
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
