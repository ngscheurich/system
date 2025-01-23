local util = require("util")

return {
  util.treesitter_ensure("css"),
  util.formatter_setup("css", { "prettier" }),
}
