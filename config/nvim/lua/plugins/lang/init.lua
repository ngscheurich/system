local util = require("util")

local spec = {
  "sheerun/vim-polyglot",
  "fladson/vim-kitty",
}

util.foreach_module("plugins.lang", function(mod)
  spec = vim.list_extend(spec, require(mod))
end)

return spec
