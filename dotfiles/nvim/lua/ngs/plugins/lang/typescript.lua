local util = require("ngs.util")

return {
  util.treesitter_ensure("typescript"),
  util.lspconfig_setup("tsserver"),
  util.conform_setup("typescript", { "prettierd" }),
}
