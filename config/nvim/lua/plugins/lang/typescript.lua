local util = require("util")

return {
  util.treesitter_ensure("typescript"),
  util.lspconfig_setup("ts_ls"),
  util.formatter_setup("typescript", { "prettier" }),
}
