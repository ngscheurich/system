local M = { "nvim-telescope/telescope.nvim" }

M.dependencies = {
  "nvim-lua/plenary.nvim",
  { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
}

function M.config()
  local telescope = require("telescope")

  telescope.setup({
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
  })

  telescope.load_extension("fzf")
end

local function find(source, theme, opts)
  return function()
    if theme then
      require("telescope.builtin")[source](require("telescope.themes")[theme](opts))
    else
      require("telescope.builtin")[source]()
    end
  end
end

M.keys = {
  { "<Leader>/", find("current_buffer_fuzzy_find", "get_ivy"), desc = "Search" },
  { "<Leader><Space>", find("buffers", "get_dropdown", { previewer = false }), desc = "Buffers" },
  { "<Leader>fa", find("autocommands"), desc = "Autocommands" },
  { "<Leader>fc", find("commands"), desc = "Commands" },
  { "<Leader>ff", find("find_files"), desc = "Files" },
  { "<Leader>fg", find("live_grep"), desc = "Grep" },
  { "<Leader>fh", find("help_tags"), desc = "Help" },
  { "<Leader>fk", find("keymaps"), desc = "Keymaps" },
  { "<Leader>fl", find("loclist", "get_ivy"), desc = "Location list" },
  { "<Leader>fo", find("oldfiles", "get_dropdown", { previewer = false }), desc = "Recent files" },
  { "<Leader>fq", find("quickfix"), desc = "Quickfix list" },
}

return M
