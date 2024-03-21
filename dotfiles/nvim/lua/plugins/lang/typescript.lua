local util = require("util")

return {
  util.treesitter_ensure("typescript"),
  util.lspconfig_setup("tsserver"),
  util.formatter_setup("typescript", { "prettierd" }),
}
