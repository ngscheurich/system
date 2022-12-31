local function builtin(source)
  require("telescope.builtin")[source]()
end

return {
  "nvim-telescope/telescope.nvim",

  dependencies = {
    "nvim-lua/plenary.nvim",
  },

  keys = {
    { "<Leader>/", function() builtin("current_buffer_fuzzy_find") end, desc = "Search" },
    { "<Leader><Space>", function() builtin("buffers") end, desc = "Buffers" },
    { "<Leader>fa", function() builtin("autocommands") end, desc = "Autocommands" },
    { "<Leader>fc", function() builtin("commands") end, desc = "Commands" },
    { "<Leader>ff", function() builtin("find_files") end, desc = "Files" },
    { "<Leader>fg", function() builtin("live_grep") end, desc = "Grep" },
    { "<Leader>fh", function() builtin("help_tags") end, desc = "Help" },
    { "<Leader>fl", function() builtin("loclist") end, desc = "Location list" },
    { "<Leader>fq", function() builtin("quickfix") end, desc = "Quickfix list" },
  }
}
