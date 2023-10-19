return {
  "folke/which-key.nvim",

  config = function()
    require("which-key")

    local mappings = {
      ["<Leader>"] = {
        ["?"] = { "<Cmd>WhichKey<CR>", "Keys" },
        e = { name = "explore" },
        f = { name = "find" },
        l = { name = "list" },
        o = { name = "outline" },
        t = { name = "test" },
        u = { name = "toggle" },
      },
    }

    require("which-key").register(mappings)
  end,
}
