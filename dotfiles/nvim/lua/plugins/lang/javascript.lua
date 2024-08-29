local util = require("util")

return {
  util.treesitter_ensure("javascript"),
  util.lspconfig_setup("tsserver"),
  util.formatter_setup("javascript", { "prettierd" }),
}
