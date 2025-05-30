return {
	"nvim-treesitter/nvim-treesitter",

	opts = {
		highlight = { enable = true },
		indent = { enable = true },
		ensure_installed = {
			"bash",
			"css",
			"elixir",
			"erlang",
			"fennel",
			"gdscript",
			"go",
			"graphql",
			"heex",
			"html",
			"http",
			"javascript",
			"json",
			"kdl",
			"lua",
			"markdown",
			"markdown_inline",
			"nix",
			"python",
			"rust",
			"scss",
			"sql",
			"svelte",
			"typescript",
			"vimdoc",
			"xml",
			"yaml",
		},
	},

	config = function(_, opts)
		require("nvim-treesitter.configs").setup(opts)
	end,
}
