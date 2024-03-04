return {
  -- Start screen
  { "echasnovski/mini.starter", version = false, config = true },

  -- Improve default UI elements
  "stevearc/dressing.nvim",

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

  -- Tabs
  {
    "akinsho/bufferline.nvim",
    version = "*",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        mode = "tabs",
        indicator = {
          icon = "┃",
        },
        always_show_bufferline = false,
        offsets = {
          {
            filetype = "neo-tree",
            text = "",
            text_align = "left",
            separator = false,
          },
        },
      },
    },
  },

  -- Indentation guide
  {
    "lukas-reineke/indent-blankline.nvim",
    cmd = { "IBLEnable", "IBLToggle" },
    config = function()
      require("ibl").setup({
        enabled = false,
        indent = { char = "┊" },
      })
      vim.keymap.set("n", "<Leader>ui", "<Cmd>IBLToggle<CR>", { desc = "Indent guide" })
    end,
  },

  -- Notifications and progress indicator
  {
    "j-hui/fidget.nvim",
    tag = "v1.0.0",
    config = true,
  },

  -- Keymap guide
  {
    "folke/which-key.nvim",
    event = "VimEnter",
    config = function()
      require("which-key")

      local mappings = {
        ["<Leader>"] = {
          ["?"] = { "<Cmd>WhichKey<CR>", "Keys" },
          e = { name = "Explore" },
          f = { name = "Find" },
          l = { name = "List" },
          o = { name = "Outline" },
          t = { name = "Test" },
          u = { name = "UI Toggles" },
        },
      }

      require("which-key").register(mappings)
    end,
  },

  -- Buffer-based file explorer
  {
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup({
        default_file_explorer = true,
      })
      vim.keymap.set("n", "-", "<Cmd>Oil<CR>", { desc = "Open parent directory" })
    end,
  },

  -- Tree-based file explorer
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
    cmd = { "Trouble", "TroubleToggle" },
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
    cmd = { "AerialToggle", "AerialNavToggle" },
    keys = {
      { "<Leader>oo", "<Cmd>AerialToggle<CR>", desc = "Panel" },
      { "<Leader>of", "<Cmd>AerialNavToggle<CR>", desc = "Float" },
    },
    config = true,
  },
}
