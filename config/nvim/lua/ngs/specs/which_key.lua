return {
	"folke/which-key.nvim",

	event = "VeryLazy",

	opts = {
		icons = {
			mappings = false,
		},
	},

	config = function(_, opts)
		local wk = require("which-key")
		wk.setup(opts)
		wk.add({
			{ "<Leader>a", group = "ai" },
			{ "<Leader>f", group = "find" },
			{ "<Leader>g", group = "git" },
			{ "<Leader>l", group = "list" },
			{ "<Leader>s", group = "search" },
			{ "<Leader>t", group = "test" },
			{ "<Leader>u", group = "ui toggles" },
		})
	end,
}
