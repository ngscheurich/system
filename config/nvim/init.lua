vim.g.ngs = {
	theme = {
		name = "default",
		colorscheme = "default",
		palette = "default",
	},
}

vim.g.mapleader = " "
vim.g.maplocalleader = ","

require("core.abbrevs")
require("core.autocmds")
require("core.commands")
require("core.diagnostic")
require("core.keymaps")
require("core.options")

vim.lsp.enable({ "lexical", "luals" })

require("core.lazy")

vim.opt.runtimepath:prepend(vim.env.HOME .. "/.theme/nvim")
local theme_ok, theme = pcall(require, "ngs_theme")
if theme_ok then
	local ngs = vim.g.ngs
	ngs.theme = theme
	vim.g.ngs = ngs
end

require("lazy").setup({
	spec = { { import = "spec" } },
	install = { colorscheme = { vim.g.ngs.theme.colorscheme } },
	checker = { enabled = true },
	change_detection = { notify = false },
})
