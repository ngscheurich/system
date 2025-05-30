local ngs_default = vim.api.nvim_create_augroup("ngs.default", {})

vim.api.nvim_create_autocmd("ColorScheme", {
	desc = "Rebuild the status line when the color scheme changes",
	group = ngs_default,
	callback = function()
		require("spec.heirline").config()
	end,
})
