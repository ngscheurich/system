local util = require("util")

return {
  util.treesitter_ensure("yaml"),
  util.lspconfig_setup("yamlls"),
}
