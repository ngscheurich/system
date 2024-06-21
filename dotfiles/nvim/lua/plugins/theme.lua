-- ===================================================================
--  Colors and Iconography
-- ===================================================================

local util = require("util")

if _G.theme.colorscheme.name == "habamax" then
  vim.cmd.colorscheme("habamax")
end

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
