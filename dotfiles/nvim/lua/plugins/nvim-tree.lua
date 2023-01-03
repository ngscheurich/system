local M = { "nvim-tree/nvim-tree.lua" }

function M.config()
  local nonicons = require("nvim-nonicons.extentions.nvim-tree")

  require("nvim-tree").setup({
    hijack_directories = { enable = false },
    renderer = {
      icons = {
        glyphs = vim.tbl_deep_extend("force", nonicons.glyphs, {
          git = {
            unstaged = "",
            staged = "",
            unmerged = "",
            renamed = "⇒",
            untracked = "?",
          },
        }),
      },
    },
  })
end

M.cmd = {
  "NvimTreeToggle",
  "NvimTreeFindFileToggle",
}

M.keys = {
  { "<Leader>ee", "<Cmd>NvimTreeToggle<CR>", desc = "Root (toggle)" },
  { "<Leader>ef", "<Cmd>NvimTreeFindFileToggle<CR>", desc = "File (toggle)" },
}

return M
