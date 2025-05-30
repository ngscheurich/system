return {
	"folke/trouble.nvim",

	cmd = "Trouble",

	opts = {},

	keys = {
		{ "grr", "<Cmd>Trouble lsp_references toggle<CR>", desc = "References" },
		{ "gO", "<Cmd>Trouble lsp_document_symbols toggle<CR>", desc = "Document symbols" },
		{ "<Leader>ld", "<Cmd>Trouble diagnostics toggle filter.buf=0<CR>", desc = "Diagnostics (buffer)" },
		{ "<Leader>lD", "<Cmd>Trouble diagnostics toggle<CR>", desc = "Diagnostics" },
		{ "<Leader>ll", "<cmd>Trouble loclist toggle<CR>", desc = "Location list" },
		{ "<Leader>lq", "<cmd>Trouble qflist toggle<CR>", desc = "Quickfix list" },
	},
}
