local util = require("ngs.util")

return {
  util.treesitter_ensure("rust"),
  util.lspconfig_setup("rust_analyzer"),
}
