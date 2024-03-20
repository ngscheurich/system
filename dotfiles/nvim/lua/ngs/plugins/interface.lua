-- ===================================================================
--  User Interface Elements
-- ===================================================================

local util = require("ngs.util")

return {
  -- =================================================================
  --  mini.starter
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-starter.md
  --  Fast and flexible start screen
  -- -----------------------------------------------------------------
  { "echasnovski/mini.starter", version = false, config = true },

  -- =================================================================
  --  Dressing.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/stevearc/dressing.nvim
  --  Improve the default `vim.ui` interfaces
  -- -----------------------------------------------------------------
  "stevearc/dressing.nvim",

  -- =================================================================
  --  incline.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/b0o/incline.nvim
  --  Show floating buffer labels
  -- -----------------------------------------------------------------
  {
    "b0o/incline.nvim",
    config = function()
      local incline = require("incline")

      incline.setup({
        hide = {
          cursorline = true,
          only_win = false,
        },
      })

      vim.keymap.set("n", "<Leader>uf", function()
        incline.toggle()
      end, { desc = "Filenames" })
    end,
  },

  -- =================================================================
  --  bufferline.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/akinsho/bufferline.nvim
  --  Decorate tabpage indicators
  -- -----------------------------------------------------------------
  {
    "akinsho/bufferline.nvim",
    version = "*",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        mode = "tabs",
        indicator = {
          icon = "┃ ",
        },
        always_show_bufferline = false,
        offsets = {
          {
            filetype = "neo-tree",
            text = "Project",
            text_align = "center",
            separator = false,
          },
        },
      },
    },
  },

  -- =================================================================
  --  Indent Blankline
  -- -----------------------------------------------------------------
  --  https://github.com/lukas-reineke/indent-blankline.nvim
  --  Indentation guides
  -- -----------------------------------------------------------------
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    config = function()
      require("ibl").setup({
        enabled = false,
        indent = { char = "┊" },
      })
      vim.keymap.set("n", "<Leader>ui", "<Cmd>IBLToggle<CR>", { desc = "Indent guide" })
    end,
  },

  -- =================================================================
  --  Fidget
  -- -----------------------------------------------------------------
  --  https://github.com/j-hui/fidget.nvim
  --  Notifications and progress indicator
  -- -----------------------------------------------------------------
  {
    "j-hui/fidget.nvim",
    tag = "v1.0.0",
    opts = {
      notification = {
        window = {
          winblend = 0,
        },
      },
    },
  },

  -- =================================================================
  --  Which Key
  -- -----------------------------------------------------------------
  --  https://github.com/folke/which-key.nvim
  --  Display possibilities for incomplete keybindings
  -- -----------------------------------------------------------------
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

  -- =================================================================
  --  oil.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/stevearc/oil.nvim
  --  Directory explorer with intuitive filesystem editing
  -- ----------------------------------------------------------------
  {
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup({
        default_file_explorer = true,
      })
      vim.keymap.set("n", "-", "<Cmd>Oil<CR>", { desc = "Open parent directory" })
    end,
  },

  -- =================================================================
  --  Neo-tree.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/nvim-neo-tree/neo-tree.nvim
  --  Tree-based file explorer
  -- -----------------------------------------------------------------
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

  -- =================================================================
  --  Trouble
  -- -----------------------------------------------------------------
  --  https://github.com/folke/trouble.nvim
  --  List viewer for diagnostics, quickfix, and more
  -- -----------------------------------------------------------------
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    cmd = { "Trouble", "TroubleToggle" },
    keys = {
      { "\\", "<Cmd>TroubleToggle<CR>", desc = "Trouble" },
    },
  },

  -- =================================================================
  --  aerial.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/stevearc/aerial.nvim
  --  Code outline window
  -- -----------------------------------------------------------------
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

  -- =================================================================
  --  barbecue.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/utilyre/barbecue.nvim
  --  Document breadcrumbs in the winbar
  -- -----------------------------------------------------------------
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons",
    },

    config = function()
      local bbq = require("barbecue")
      local function setup_barbecue()
        local bg_color = util.get_highlight_group_attr("Cursorline", "bg#")
        bbq.setup({
          show_dirname = false,
          show_modified = true,
          theme = { normal = { bg = bg_color } },
        })
      end

      setup_barbecue()
      vim.cmd("Barbecue hide")

      vim.api.nvim_create_autocmd("ColorScheme", {
        desc = "Updates breadcrumbs colors on colorscheme change",
        pattern = "*",
        callback = setup_barbecue,
      })

      vim.keymap.set("n", "<Leader>ub", function()
        local ok, incline = pcall(require, "incline")
        if ok then
          incline.toggle()
        end
        vim.cmd("Barbecue toggle")
      end, { desc = "Breadcrumbs" })
    end,
  },

  -- =================================================================
  --  nvim-scrollbar
  -- -----------------------------------------------------------------
  --  https://github.com/petertriho/nvim-scrollbar
  --  Scrollbar with diagnostics indicators
  -- -----------------------------------------------------------------
  {
    "petertriho/nvim-scrollbar",
    config = true,
  },

  -- =================================================================
  --  highlight-undo.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/tzachar/highlight-undo.nvim
  --  Highlight undo/redo operations
  -- -----------------------------------------------------------------
  {
    "tzachar/highlight-undo.nvim",
    config = true,
  },
}
