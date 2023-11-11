local util = require("ngs.util")

local spec = {
  "sheerun/vim-polyglot",
}

util.foreach_module("ngs.plugins.lang", function(mod)
  spec = vim.list_extend(spec, require(mod))
end)

return spec