local util = require("util")

return {
  util.treesitter_ensure("nix"),
  util.lspconfig_setup("rnix"),
}
