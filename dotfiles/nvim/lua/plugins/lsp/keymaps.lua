return {
  on_attach = function(client, buffer)
    local lsp = vim.lsp
    local telescope = require("telescope.builtin")
    local formatting = client.supports_method("textDocument/formatting")

    local mappings = {
      ["<LocalLeader>"] = {
        name = "lsp",

        R = { ":IncRename ", "Rename" },
        S = { telescope.lsp_workspace_symbols, "Symbols" },
        a = { lsp.buf.code_action, "Code actions" },
        d = { vim.diagnostic.open_float, "Diagnostics" },
        f = { lsp.buf.format, "Format", cond = formatting },
        o = { "<Cmd>SymbolsOutline<CR>", "Outline" },
        r = { telescope.lsp_references, "References" },
        s = { telescope.lsp_document_symbols, "Symbols" },
      },

      K = { lsp.buf.hover, "Hover" },
      ["<C-]>"] = { lsp.buf.definition, "Go to definition" },
      ["[d"] = { lsp.diagnostic.goto_prev, "Prev diagnostic" },
      ["]d"] = { lsp.diagnostic.goto_next, "Next diagnostic" },
    }

    require("which-key").register(mappings, { buffer = buffer })
  end,
}