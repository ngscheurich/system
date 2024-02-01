local pick = require("ngs.util").pick

return {
  "nvim-telescope/telescope.nvim",

  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },

  opts = {
    defaults = {
      prompt_prefix = " ❯ ",
      selection_caret = " ❯ ",
      entry_prefix = "   ",
    },
    extensions = {
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
      },
    },
  },

  keys = {
    { "<C-F>", pick("find_files"), desc = "Find files" },

    { "<Leader>/", pick("current_buffer_fuzzy_find", "get_ivy"), desc = "Search" },
    { "<Leader><Space>", pick("buffers", "get_dropdown", { previewer = false }), desc = "Buffers" },
    { "<Leader>fa", pick("autocommands"), desc = "Autocommands" },
    { "<Leader>fc", pick("commands"), desc = "Commands" },
    { "<Leader>ff", pick("find_files"), desc = "Files" },
    { "<Leader>fg", pick("live_grep"), desc = "Grep" },
    { "<Leader>fh", pick("help_tags"), desc = "Help" },
    { "<Leader>fk", pick("keymaps"), desc = "Keymaps" },
    { "<Leader>fl", pick("loclist", "get_ivy"), desc = "Location list" },
    { "<Leader>fo", pick("oldfiles", "get_dropdown", { previewer = false }), desc = "Recent files" },
    { "<Leader>fq", pick("quickfix"), desc = "Quickfix list" },
  },
}
