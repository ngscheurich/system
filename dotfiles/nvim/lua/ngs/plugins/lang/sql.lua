local util = require("ngs.util")

return {
  util.treesitter_ensure("sql"),
  util.lspconfig_setup("sqlls"),
  util.conform_setup("sql", { "sql_formatter" }),
}
