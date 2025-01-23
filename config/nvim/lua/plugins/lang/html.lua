local util = require("util")

return {
  util.treesitter_ensure("html"),
  util.formatter_setup("html", { "prettier" }),
}
