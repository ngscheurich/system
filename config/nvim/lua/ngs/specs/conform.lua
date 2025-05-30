return {
	"stevearc/conform.nvim",

	event = "VeryLazy",

	opts = {
		format_on_save = {
			timeout_ms = 500,
			lsp_fallback = true,
		},
		formatters_by_ft = {
			css = { "prettier" },
			fennel = { "fnlfmt" },
			html = { "prettier" },
			http = { "kulala-fmt" },
			javascript = { "prettier" },
			json = { "prettier" },
			lua = { "stylua" },
			python = { "black" },
			scss = { "prettier" },
			sql = { "sleek" },
			typescript = { "prettier" },
		},
	},
}
