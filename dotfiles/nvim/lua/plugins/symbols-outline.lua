return {
  "simrat39/symbols-outline.nvim",

  cmd = "SymbolsOutline",

  keys = {
    { "<Leader>To", "<Cmd>SymbolsOutline<CR>", desc = "Outline" },
  },

  config = {
    symbols = {
      Field = { icon = "⊞" },
      Interface = { icon = "" },
      Function = { icon = "ƒ" },
      Constant = { icon = "π" },
      String = { icon = "" },
    },
  },
}
