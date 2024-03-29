-- ===================================================================
--  Colors and Iconography
-- ===================================================================

return {
  -- =================================================================
  --  Catppuccin
  -- -----------------------------------------------------------------
  --  https://github.com/catppuccin/catppuccin
  --  Comprehensive, customizable colorscheme
  -- -----------------------------------------------------------------
  {
    "catppuccin/nvim",
    name = "catppuccin",
    cond = vim.startswith(_G.theme.colorscheme.name, "catppuccin"),
    lazy = false,
    priority = 1000,
    config = function()
      local colorscheme = _G.theme.colorscheme
      local opts = {
        integrations = {
          aerial = true,
          fidget = true,
          leap = true,
          mason = true,
          neotree = true,
          lsp_trouble = true,
          neotest = true,
          which_key = true,
        },
      }
      require("catppuccin").setup(vim.tbl_extend("error", opts, colorscheme.opts))
      vim.cmd.colorscheme(colorscheme.name)
    end,
  },

  -- =================================================================
  --  Tokyo Night
  -- -----------------------------------------------------------------
  --  https://github.com/folke/tokyonight.nvim
  --  Cool, dark colorschema
  -- -----------------------------------------------------------------
  {
    "folke/tokyonight.nvim",
    cond = vim.startswith(_G.theme.colorscheme.name, "tokyonight"),
    lazy = false,
    priority = 1000,
    config = function()
      require("tokyonight").setup({
        sidebars = { "qf", "terminal", "help" },
      })
      vim.cmd.colorscheme(_G.theme.colorscheme.name)
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
