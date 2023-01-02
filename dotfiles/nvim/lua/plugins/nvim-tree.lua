local M = { "nvim-tree/nvim-tree.lua" }

function M.config()
  require("nvim-tree").setup({
    hijack_directories = { enable = false },
    renderer = {
      icons = {
        glyphs = require("nvim-nonicons.extentions.nvim-tree").glyphs,
      },
    },
  })
end

M.cmd = {
  "NvimTreeToggle",
  "NvimTreeFindFileToggle"
}

M.keys = {
  { "<Leader>ee", "<Cmd>NvimTreeToggle<CR>", desc = "Root (toggle)" },
  { "<Leader>ef", "<Cmd>NvimTreeFindFileToggle<CR>", desc = "File (toggle)" },
}

return M
