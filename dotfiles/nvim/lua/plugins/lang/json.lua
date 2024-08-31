local util = require("util")

return {
  util.treesitter_ensure("json"),
  util.formatter_setup("json", { "prettier" }),
}
