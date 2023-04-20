local M = { "nvim-neotest/neotest" }

M.dependencies = {
  "antoinemadec/FixCursorHold.nvim",
  "jfpedroza/neotest-elixir",
  "nvim-lua/plenary.nvim",
  "nvim-treesitter/nvim-treesitter",
}

function M.config()
  require("neotest").setup({
    adapters = {
      require("neotest-elixir")({}),
    },
  })
end

local function test(func, opts)
  if opts == nil then
    opts = {}
  end

  return function()
  require("neotest").run[func](opts)
  end
end

M.keys = {
  { "<Leader>tn", test("run"), desc = "Nearest" },
  { "<Leader>tN", test("run", { strategy = "dap" }), desc = "Nearest (Debug)" },
  { "<Leader>tl", test("run_last"), desc = "Last" },
  { "<Leader>tL", test("run_last", { strategy = "dap" }), desc = "Last (Debug)" },
}

return M
