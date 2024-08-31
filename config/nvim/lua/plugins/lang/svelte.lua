local util = require("util")

return {
  util.treesitter_ensure("svelte"),
  util.lspconfig_setup("svelte"),
}
