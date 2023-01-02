local M = { "nvim-telescope/telescope.nvim" }

local function find(source)
  return function()
    require("telescope.builtin")[source]()
  end
end

M.dependencies = { "nvim-lua/plenary.nvim" }

function M.config()
  require("telescope").setup({
    defaults = {
      prompt_prefix = " ❯ ",
      selection_caret = " ❯ ",
      entry_prefix = "   ",
    },
  })
end

M.keys = {
  { "<Leader>/", find("current_buffer_fuzzy_find"), desc = "Search" },
  { "<Leader><Space>", find("buffers"), desc = "Buffers" },
  { "<Leader>fa", find("autocommands"), desc = "Autocommands" },
  { "<Leader>fc", find("commands"), desc = "Commands" },
  { "<Leader>ff", find("find_files"), desc = "Files" },
  { "<Leader>fg", find("live_grep"), desc = "Grep" },
  { "<Leader>fh", find("help_tags"), desc = "Help" },
  { "<Leader>fk", find("keymaps"), desc = "Keymaps" },
  { "<Leader>fl", find("loclist"), desc = "Location list" },
  { "<Leader>fq", find("quickfix"), desc = "Quickfix list" },
}

return M
