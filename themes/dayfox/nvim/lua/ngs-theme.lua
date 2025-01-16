return {
	name = "dayfox",

	colorscheme = {
		name = "dayfox",
		opts = {},
	},

	plugins = {},

	callback = function()
		vim.cmd("hi CursorLine guibg=#e4dcd4")
	end,
}
