-- ===================================================================
--  Kanagawa
-- -------------------------------------------------------------------
--  https://github.com/rebelot/kanagawa.nvim
--  Colorscheme inspired by _The Great Wave off Kanagawa_
--  ui
-- -------------------------------------------------------------------
return {
	"rebelot/kanagawa.nvim",

	lazy = false,
	priority = 1000,

	config = function()
		require("kanagawa").setup({
			colors = { palette = require("ngs.specs.kanagawa." .. vim.g.ngs.theme.palette) },
			overrides = function(colors)
				local theme = colors.theme

				local higroups = {
					NormalDark = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m3 },
				}

				local bg_none = { "NormalFloat", "FloatBorder", "FloatTitle" }
				for _, h in ipairs(bg_none) do
					higroups[h] = { bg = "none" }
				end

				local bg_dark = { "LazyNormal", "NeotreeNormal", "NeotreeNormalNC" }
				for _, h in ipairs(bg_dark) do
					higroups[h] = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim }
				end

				return higroups
			end,
		})

		vim.cmd.colorscheme(vim.g.ngs.theme.colorscheme)
	end,
}
