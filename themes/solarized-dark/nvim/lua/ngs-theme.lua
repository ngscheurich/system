return {
	name = "solarized-dark",

	colorscheme = {
		name = "solarized",
		opts = {
			variant = "winter",
		},
	},

	plugins = {
		lualine = {
			theme = "solarized",
		},
	},

	callback = function()
		vim.o.background = "dark"
	end,
}
