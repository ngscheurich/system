vim.g.ngs = {
	theme = {
		name = "default",
		colorscheme = "default",
		palette = "default",
	},
}

vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.lsp.enable({ "lexical", "luals" })

require("ngs.core.abbrevs")
require("ngs.core.autocmds")
require("ngs.core.commands")
require("ngs.core.diagnostic")
require("ngs.core.keymaps")
require("ngs.core.options")
require("ngs.core.lazy")

require("ngs.util").load_theme()

require("lazy").setup({
	spec = { { import = "ngs.specs" } },
	install = { colorscheme = { vim.g.ngs.theme.colorscheme } },
	checker = { enabled = true },
	change_detection = { notify = false },
})
