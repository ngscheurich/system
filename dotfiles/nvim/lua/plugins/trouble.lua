local M = { "folke/trouble.nvim" }

M.config = {
  signs = {
    other = "",
  },
}

local function list(arg)
  return "<Cmd>Trouble " .. arg .. "<CR>"
end

M.keys = {
  { "\\", "<Cmd>TroubleToggle<CR>", desc = "Toggle list" },

  { "<Leader>lD", list("lsp_definitions"), desc = "Definitions" },
  { "<Leader>ld", list("document_diagnostics"), desc = "Document diagnostics" },
  { "<Leader>ll", list("loclist"), desc = "Location" },
  { "<Leader>lq", list("quickfix"), desc = "Quickfix" },
  { "<Leader>lr", list("lsp_references"), desc = "References" },
  { "<Leader>lt", list("lsp_type_definitions"), desc = "Type definitions" },
  { "<Leader>lw", list("workspace_diagnostics"), desc = "Workspace diagnostics" },
}

return M
