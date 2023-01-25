return {
  "nvim-neotest/neotest",

  dependencies = {
    "antoinemadec/FixCursorHold.nvim",
    "jfpedroza/neotest-elixir",
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
  },

  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-elixir")({}),
      },
    })
  end,
}
