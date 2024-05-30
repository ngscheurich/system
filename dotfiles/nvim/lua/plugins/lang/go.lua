local util = require("util")

return {
  util.treesitter_ensure("go"),
  util.lspconfig_setup("gopls"),
}
