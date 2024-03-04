-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- ///// \/// \\/// \///// \///////// \\\\\\/// \\\\\\\/// \\/// \\/////// \\
-- \/// \\///// /// \\/// \\\\\/// \\\\\\\\\/// \\\\\\\/// \\/// \/// \\/// \
-- \/// \\///////// \\/// \\\\\/// \\\\\\\\\/// \\\\\\\/// \\/// \///////// \
-- \/// \\/// ///// \\/// \\\\\/// \\\\\\\\\/// \\\\\\\/// \\/// \/// \\/// \
-- ///// \/// \\/// \///// \\\\/// \\\\/// \///////// \\/////// \\/// \\/// \
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

local util = require("ngs.util")

-- Bootstrap plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end

vim.opt.runtimepath:prepend(lazypath)

-- Load theme setting
vim.opt.runtimepath:prepend(vim.env.HOME .. "/.theme/nvim")

local theme = require("ngs-theme")
vim.g.theme = theme.name
vim.g.colorscheme = theme.colorscheme or theme.name

-- Load core config
util.foreach_module("ngs.core", function(mod)
  require(mod)
end)

-- Load plugin manager
require("lazy").setup("ngs.plugins", {
  change_detection = { notify = false },
  install = {
    colorscheme = { vim.g.colorscheme },
  },
})
