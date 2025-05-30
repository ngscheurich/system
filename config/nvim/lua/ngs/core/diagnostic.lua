vim.diagnostic.config({
	signs = {
		text = {
			["vim.diagnostic.severity.ERROR"] = "",
			["vim.diagnostic.severity.WARN"] = "",
			["vim.diagnostic.severity.INFO"] = "",
			["vim.diagnostic.severity.HINT"] = "",
		},
	},
})

vim.keymap.set("n", "<Leader>d", vim.diagnostic.open_float, { desc = "Show diagnostics" })
