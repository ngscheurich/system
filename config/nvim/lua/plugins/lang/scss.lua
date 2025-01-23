local util = require("util")

return {
  util.treesitter_ensure("scss"),
  util.formatter_setup("scss", { "prettier" }),
}
