local util = require("util")

return {
  util.treesitter_ensure("sql"),
  util.lspconfig_setup("sqlls"),
  util.formatter_setup("sql", { "sleek" }),
  util.linter_setup("sql", { "sqlfluff" }),
}
