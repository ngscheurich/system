local util = require("ngs.util")

return {
  util.treesitter_ensure("nix"),
  util.lspconfig_setup("rnix"),
}
