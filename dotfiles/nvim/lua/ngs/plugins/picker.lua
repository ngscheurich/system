local pick = require("ngs.util").pick

return {
  "nvim-telescope/telescope.nvim",
  event = "VimEnter",
  branch = "0.1.x",

  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
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

    telescope.setup({
      defaults = {
        prompt_prefix = " ❯ ",
        selection_caret = " ❯ ",
        entry_prefix = "   ",
      },
      extensions = {
        ["ui-select"] = {
          require("telescope.themes").get_dropdown(),
        },
      },
    })

    pcall(telescope.load_extension, "fzf")
    pcall(telescope.load_extension, "ui-select")

    -- mappings = {
    --   i = {
    --     ["<M-k>"] = require("telescope.actions").cycle_history_next,
    --     ["<M-j>"] = require("telescope.actions").cycle_history_prev,
    --   },
    -- },
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
    { "<Leader>fq", pick("quickfix"), desc = "Quickfix list" },
  },
}
