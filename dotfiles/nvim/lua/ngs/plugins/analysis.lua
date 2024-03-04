return {
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {},
    },
    config = function(_, opts)
      local lint = require("lint")
      for ft, linters in pairs(opts.linters_by_ft) do
        lint.linters_by_ft = vim.tbl_extend("error", lint.linters_by_ft, {
          [ft] = linters,
        })
      end
    end,
  },
}
