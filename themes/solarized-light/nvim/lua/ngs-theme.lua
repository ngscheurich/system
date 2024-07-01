return {
	name = "solarized-light",

	colorscheme = {
		name = "solarized",
		opts = {},
	},

	plugins = {
		lualine = {
			theme = "solarized",
		},
	},

	callback = function()
		vim.o.background = "light"
	end,
}
