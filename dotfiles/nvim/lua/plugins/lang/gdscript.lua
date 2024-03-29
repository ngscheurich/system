local util = require("util")

return {
  util.treesitter_ensure("gdscript"),
  util.lspconfig_setup("gdscript"),
}
