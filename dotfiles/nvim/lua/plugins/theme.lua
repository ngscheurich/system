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
    cond = vim.startswith(vim.g.theme, "catppuccin"),
    lazy = false,
    priority = 1000,
    config = function()
      local palettes = require("plugins.theme.catppuccin_gruvbox")
      require("catppuccin").setup({
        background = {
          light = "latte",
          dark = "mocha",
        },
        color_overrides = {
          latte = palettes.latte,
          mocha = palettes.mocha,
        },
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
      })
      vim.cmd.colorscheme(vim.g.colorscheme)
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
    cond = vim.startswith(vim.g.theme, "tokyonight"),
    lazy = false,
    priority = 1000,
    config = function()
      require("tokyonight").setup({
        sidebars = { "qf", "terminal", "help" },
      })
      vim.cmd.colorscheme(vim.g.colorscheme)
    end,
  },

  -- =================================================================
  --  Modus Themes
  -- -----------------------------------------------------------------
  --  https://github.com/miikanissi/modus-themes.nvim
  --  Highly accessible colorschemes
  -- -----------------------------------------------------------------
  {
    "miikanissi/modus-themes.nvim",
    cond = vim.startswith(vim.g.theme, "modus"),
    lazy = false,
    priority = 1000,
    config = function()
      require("modus-themes").setup({
        variant = "tinted",
      })
      vim.cmd.colorscheme(vim.g.colorscheme)
    end,
  },

  -- =================================================================
  --  Rosé Pine
  -- -----------------------------------------------------------------
  --  https://github.com/rose-pine/neovim
  --  Classy, minimalist colorscheme
  -- -----------------------------------------------------------------
  {
    "rose-pine/neovim",
    name = "rose-pine",
    cond = vim.startswith(vim.g.theme, "rose-pine"),
    lazy = false,
    priority = 1000,
    config = function()
      require("rose-pine").setup({
        styles = {
          italic = false,
        },
      })
      vim.cmd.colorscheme(vim.g.colorscheme)
    end,
  },

  -- =================================================================
  --  Solarized
  -- -----------------------------------------------------------------
  --  https://github.com/maxmx03/solarized.nvim
  --  Precision colors for machines and people
  -- -----------------------------------------------------------------
  {
    "maxmx03/solarized.nvim",
    cond = vim.startswith(vim.g.theme, "solarized"),
    lazy = false,
    priority = 1000,
    config = function()
      if vim.g.theme == "solarized-light" then
        vim.o.background = "light"
      else
        vim.o.background = "dark"
      end
      vim.cmd.colorscheme(vim.g.colorscheme)
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
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },
}
