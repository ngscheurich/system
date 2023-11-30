return {
  -- Start screen
  { "echasnovski/mini.starter", version = false, config = true },

  -- Notifications
  "rcarriga/nvim-notify",

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

  -- Progress indicator
  {
    "j-hui/fidget.nvim",
    branch = "legacy",
    config = true,
  },
  -- Keymap guide
  {
    "folke/which-key.nvim",

    config = function()
      require("which-key")

      local mappings = {
        ["<Leader>"] = {
          ["?"] = { "<Cmd>WhichKey<CR>", "Keys" },
          e = { name = "explore" },
          f = { name = "find" },
          l = { name = "list" },
          o = { name = "outline" },
          t = { name = "test" },
          u = { name = "toggle" },
        },
      }

      require("which-key").register(mappings)
    end,
  },

  -- Tree-based explorer
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
        "<Leader>ef",
        "<Cmd>Neotree action=focus source=filesystem position=left reveal=true<CR>",
        desc = "Files",
      },
      {
        "<Leader>eb",
        "<Cmd>Neotree action=focus source=buffers position=left<CR>",
        desc = "Buffers",
      },
      {
        "<Leader>eg",
        "<Cmd>Neotree action=focus source=git_status position=left<CR>",
        desc = "Git status",
      },
      {
        "<Leader>eF",
        "<Cmd>Neotree action=focus source=filesystem position=float reveal=true<CR>",
        desc = "Files (Floating)",
      },
    },
    opts = {
      close_if_last_window = true,
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

  -- Useful lists
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    commands = { "Trouble", "TroubleToggle" },
    keys = {
      { "\\", "<Cmd>TroubleToggle<CR>", desc = "Trouble" },
    },
  },

  -- Code outline
  {
    "stevearc/aerial.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    commands = { "AerialToggle", "AerialNavToggle" },
    keys = {
      { "<Leader>oo", "<Cmd>AerialToggle<CR>", desc = "Panel" },
      { "<Leader>of", "<Cmd>AerialNavToggle<CR>", desc = "Float" },
    },
    config = true,
  },
}
