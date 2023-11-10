return {
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
        desc = "Sidebar",
      },
      {
        "<Leader>ed",
        "<Cmd>Neotree action=focus source=filesystem position=bottom toggle=true reveal=true<CR>",
        desc = "Sidebar",
      },
      {
        "<Leader>eb",
        "<Cmd>Neotree action=focus source=buffers position=float toggle=true reveal=true<CR>",
        desc = "Float",
      },
      {
        "<Leader>eg",
        "<Cmd>Neotree action=focus source=git_status position=float toggle=true reveal=true<CR>",
        desc = "Bottom",
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
