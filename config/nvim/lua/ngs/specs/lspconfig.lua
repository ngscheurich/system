return {
  "neovim/nvim-lspconfig",

  config = function()
    vim.lsp.config("lexical", {
      cmd = { "lexical" },
    })

    vim.lsp.enable({
      "gleam",
      "lexical",
      "lua_ls",
      "ts_ls",
    })
  end,
}
