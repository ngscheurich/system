return {
  "nvim-neotest/neotest",
  dependencies = { "jfpedroza/neotest-elixir" },
  keys = {
    {
      "<Leader>tn",
      function()
        require("neotest").run.run()
      end,
      desc = "Nearest",
    },
    {
      "<Leader>tt",
      function()
        require("neotest").run.run_last()
      end,
      desc = "Last",
    },
    {
      "<Leader>tf",
      function()
        require("neotest").run.run(vim.fn.expand("%"))
      end,
      desc = "File",
    },
    {
      "<Leader>ts",
      function()
        require("neotest").summary.toggle()
      end,
      desc = "Summary",
    },
  },
  config = function()
    ---@diagnostic disable-next-line: missing-fields
    require("neotest").setup({
      adapters = {
        require("neotest-elixir"),
      },
    })
  end,
}
