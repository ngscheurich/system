-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- ///// \/// \\/// \///// \///////// \\\\\\/// \\\\\\\/// \\/// \\/////// \\
-- \/// \\///// /// \\/// \\\\\/// \\\\\\\\\/// \\\\\\\/// \\/// \/// \\/// \
-- \/// \\///////// \\/// \\\\\/// \\\\\\\\\/// \\\\\\\/// \\/// \///////// \
-- \/// \\/// ///// \\/// \\\\\/// \\\\\\\\\/// \\\\\\\/// \\/// \/// \\/// \
-- ///// \/// \\/// \///// \\\\/// \\\\/// \///////// \\/////// \\/// \\/// \
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

local util = require("util")

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
_G.theme = require("ngs-theme")

-- Load core config
util.foreach_module("core", function(mod)
  require(mod)
end)

-- Load plugin manager
require("lazy").setup("plugins", {
  change_detection = { notify = false },
  install = {
    colorscheme = { vim.g.colorscheme },
  },
})

-- Scratch
local function send_markdown_code_block()
  local node = vim.treesitter.get_node()

  if node then
    local range = vim.treesitter.get_range(node)
    local first, _, _, last, _, _ = unpack(range)
    vim.cmd(string.format("%s,%sSlimeSend", first + 1, last))
  end
end

vim.keymap.set("n", "<C-C><C-C>", send_markdown_code_block)
