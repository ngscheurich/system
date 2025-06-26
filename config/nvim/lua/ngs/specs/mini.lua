return {
	{ "echasnovski/mini.align", version = "*", event = "VeryLazy", opts = {} },
	{ "echasnovski/mini.bracketed", version = "*", event = "VeryLazy", opts = {} },
	{ "echasnovski/mini.pairs", version = "*", event = "VeryLazy", opts = {} },

	{
		"echasnovski/mini.surround",
		version = "*",
		event = "VeryLazy",

		opts = {
			mappings = {
				add = "gsa",
				delete = "gsd",
				find = "gsf",
				find_left = "gsF",
				highlight = "gsh",
				replace = "gsr",
				update_n_lines = "gsn",
				suffix_last = "l",
				suffix_next = "n",
			},
		},
	},
}
