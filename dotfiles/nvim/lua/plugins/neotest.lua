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

  keys = {
    {"<Leader>tn", function () require("neotest").run.run() end, desc = "Nearest"},
    {"<Leader>tn", function () require("neotest").run.run({strategy = "dap"}) end, desc = "Nearest (Debug)"},
    {"<Leader>tl", function () require("neotest").run.run_last() end, desc = "Last"},
    {"<Leader>tl", function () require("neotest").run.run_last({strategy = "dap"}) end, desc ="Last (Debug)"},
  }
}
