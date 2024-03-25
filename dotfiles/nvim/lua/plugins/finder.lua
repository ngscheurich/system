-- ===================================================================
--  Fuzzy Finder
-- ===================================================================

local pick = require("util").pick

-- ===================================================================
--  Telescope
-- -------------------------------------------------------------------
--  https://github.com/nvim-telescope/telescope.nvim
--  Extensiblem, general-use fuzzy finder
-- -------------------------------------------------------------------
return {
  "nvim-telescope/telescope.nvim",
  event = "VimEnter",
  branch = "0.1.x",

  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    "MunifTanjim/nui.nvim",
    "folke/which-key.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      cond = function()
        return vim.fn.executable("make") == 1
      end,
    },
  },

  config = function()
    local telescope = require("telescope")
    local actions = require("telescope.actions")
    local layout_actions = require("telescope.actions.layout")

    telescope.setup({
      defaults = {
        prompt_prefix = " ❯ ",
        selection_caret = " ❯ ",
        entry_prefix = "   ",
        mappings = {
          n = {
            ["n"] = actions.cycle_history_next,
            ["p"] = actions.cycle_history_prev,
            ["P"] = layout_actions.toggle_preview,
          },
        },
        file_ignore_patterns = {
          "assets/vendor/heroicons",
        },
      },
      pickers = {
        buffers = {
          mappings = {
            n = {
              ["dd"] = actions.delete_buffer,
            },
          },
        },
      },
      extensions = {
        ["ui-select"] = {
          require("telescope.themes").get_dropdown(),
        },
      },
    })

    pcall(telescope.load_extension, "fzf")
    pcall(telescope.load_extension, "ui-select")

    require("which-key").register({ ["<Leader>fG"] = "Git" })
  end,

  keys = {
    { "<C-F>", pick("find_files"), desc = "Find files" },
    { "<Leader>/", pick("current_buffer_fuzzy_find", "get_ivy"), desc = "Search" },
    { "<Leader><Space>", pick("buffers", "get_dropdown", { previewer = false }), desc = "Buffers" },
    { "<Leader>fa", pick("autocommands"), desc = "Autocommands" },
    { "<Leader>fc", pick("commands"), desc = "Commands" },
    { "<Leader>fd", pick("diagnostics", "get_dropdown", { previewer = false }), desc = "Diagnostics" },
    { "<Leader>ff", pick("find_files"), desc = "Files" },
    { "<Leader>fg", pick("live_grep"), desc = "Grep" },
    { "<Leader>fh", pick("help_tags"), desc = "Help" },
    { "<Leader>fk", pick("keymaps"), desc = "Keymaps" },
    { "<Leader>fl", pick("loclist", "get_ivy"), desc = "Location list" },
    { "<Leader>fr", pick("oldfiles", "get_dropdown", { previewer = false }), desc = "Recent files" },
    { "<Leader>ft", pick("treesitter"), desc = "Tree-sitter symbols" },
    { "<Leader>fq", pick("quickfix"), desc = "Quickfix list" },

    { "<Leader>fGb", pick("git_bcommits"), desc = "Buffer commits" },
    { "<Leader>fGc", pick("git_bcommits"), desc = "Commits" },
    { "<Leader>fGs", pick("git_status"), desc = "Status" },
  },
}
