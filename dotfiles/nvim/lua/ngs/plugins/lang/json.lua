local util = require("ngs.util")

return {
  util.treesitter_ensure("json"),
  util.formatter_setup("json", { "prettierd" }),
}
