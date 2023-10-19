return {
  -- Start screen
  { "echasnovski/mini.starter",    version = false, config = true },

  -- Icons
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons",      config = true },

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
    end
  },

  -- File tree
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    keys = {
      {
        "<Leader>ee",
        "<Cmd>Neotree action=focus source=filesystem position=left toggle=true reveal=true<CR>",
        desc =
        "Sidebar"
      },
      {
        "<Leader>ed",
        "<Cmd>Neotree action=focus source=filesystem position=bottom toggle=true reveal=true<CR>",
        desc =
        "Sidebar"
      },
      {
        "<Leader>eb",
        "<Cmd>Neotree action=focus source=buffers position=float toggle=true reveal=true<CR>",
        desc =
        "Float"
      },
      {
        "<Leader>eg",
        "<Cmd>Neotree action=focus source=git_status position=float toggle=true reveal=true<CR>",
        desc =
        "Bottom"
      },
    },
    opts = {
      default_component_configs = {
        indent = {
          with_expanders = true,
          with_markers = false,
        },
      },
      filesystem = {
        hijack_netrw_behavior = "disabled",
      },
      window = { width = 30 },
      source_selector = { winbar = true },
    },
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

  -- Useful lists
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    commands = { "Trouble", "TroubleToggle" },
    keys = {
      { "\\", "<Cmd>TroubleToggle<CR>", desc = "Trouble" },
    }
  },

  -- Code outline
  {
    "stevearc/aerial.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons"
    },
    commands = { "AerialToggle", "AerialNavToggle" },
    keys = {
      { "<Leader>oo", "<Cmd>AerialToggle<CR>",    desc = "Panel" },
      { "<Leader>of", "<Cmd>AerialNavToggle<CR>", desc = "Float" },
    },
    config = true,
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
    end
  },
}
