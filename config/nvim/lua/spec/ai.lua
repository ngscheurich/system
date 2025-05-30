return {
	{
		"GeorgesAlkhouri/nvim-aider",
		cmd = "Aider",
		opts = {},
		keys = {
			{ "<Leader>a/", "<Cmd>Aider toggle<CR>", desc = "Toggle" },
			{ "<Leader>aa", "<Cmd>Aider command<CR>", desc = "Commands" },
			{ "<Leader>as", "<Cmd>Aider send<CR>", desc = "Send", mode = { "n", "v" } },
			{ "<Leader>ab", "<Cmd>Aider buffer<CR>", desc = "Send buffer" },
			{ "<Leader>a+", "<Cmd>Aider add<CR>", desc = "Add file" },
			{ "<Leader>a-", "<Cmd>Aider drop<CR>", desc = "Drop File" },
			{ "<Leader>ar", "<Cmd>Aider add readonly<CR>", desc = "Add read-only" },
			{ "<Leader>aR", "<Cmd>Aider reset<CR>", desc = "Reset session" },
		},
	},

	{
		"ravitemer/mcphub.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		build = "npm install -g mcp-hub@latest",
		opts = {},
		keys = {
			{ "<Leader>am", "<Cmd>MCPHub<CR>", desc = "MCPHub" },
		},
	},
}
