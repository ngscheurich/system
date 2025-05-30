return {
	"lewis6991/gitsigns.nvim",
	config = function()
		local gs = require("gitsigns")
		local sb_ok, sb_handler = pcall(require, "scrollbar.handlers.search")

		gs.setup({
			signs = {
				add = { text = "┃" },
				change = { text = "┃" },
				changedelete = { text = "┃" },
				delete = { text = "┃" },
				topdelete = { text = "┃" },
				untracked = { text = "┇" },
			},
		})

		vim.keymap.set("n", "<Leader>gb", gs.toggle_current_line_blame, { desc = "Line blame (toggle)" })
		vim.keymap.set("n", "<Leader>gd", gs.toggle_deleted, { desc = "Deleted (toggle)" })
		vim.keymap.set("n", "<Leader>gh", gs.toggle_linehl, { desc = "Line highlight (toggle)" })
		vim.keymap.set("n", "<Leader>gp", gs.preview_hunk, { desc = "Preview hunk" })
		vim.keymap.set("n", "<Leader>gr", gs.reset_hunk, { desc = "Reset hunk" })

		if sb_ok then
			sb_handler.setup()
		end
	end,
}
