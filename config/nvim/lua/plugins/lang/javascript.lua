local util = require("util")

return {
  util.treesitter_ensure("javascript"),
  util.lspconfig_setup("ts_ls"),
  util.formatter_setup("javascript", { "prettier" }),
}
