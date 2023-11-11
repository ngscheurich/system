return {
  -- Start screen
  { "echasnovski/mini.starter", version = false, config = true },

  -- Icons
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons", config = true },

  -- Notifications
  "rcarriga/nvim-notify",

  -- Progress indicator
  {
    "j-hui/fidget.nvim",
    branch = "legacy",
    config = true,
  },

  -- Colorscheme
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

  -- Buffer labels
  {
    "b0o/incline.nvim",
    opts = {
      hide = {
        cursorline = true,
        only_win = true,
      },
    },
  },

  -- Highlight color codes
  {
    "NvChad/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    ft = { "html", "css", "javascript", "svg", "typescript" },
    opts = {
      user_default_options = {
        AARRGGBB = true,
        RGB = true,
        RRGGBB = true,
        RRGGBBAA = true,
        hsl_fn = true,
        names = false,
        rgb_fn = true,
      },
    },
  },

  -- Indentation guide
  {
    "lukas-reineke/indent-blankline.nvim",
    commands = { "IBLEnable", "IBLToggle" },
    config = function()
      require("ibl").setup({
        enabled = false,
        indent = { char = "┊" },
      })
      vim.keymap.set("n", "<Leader>ui", "<Cmd>IBLToggle<CR>", { desc = "Indent guide" })
    end,
  },
}
