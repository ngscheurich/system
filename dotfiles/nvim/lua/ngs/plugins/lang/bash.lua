local util = require("ngs.util")

return {
  util.treesitter_ensure("bash"),
  util.lspconfig_setup("bashls"),
  util.formatter_setup("sh", { "shfmt" }),
  util.linter_setup("sh", { "shellcheck" }),
}
