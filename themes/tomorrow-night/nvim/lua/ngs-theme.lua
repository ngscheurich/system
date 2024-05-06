local lighter = "#212326"
local darker = "#1b1c1f"

return {
	name = "tomorrow-night",

	lualine = "auto",

	colorscheme = {
		name = "base16-tomorrow-night",
		opts = {
			telescope = false,
		},
		plugins = {
			barbecue = {
				bg_color = darker,
			},
		},
	},

	callback = function()
		local light_bg = {
			"CursorLineNr",
			"GitGutterAdd",
			"GitGutterChange",
			"GitGutterChangeDelete",
			"GitGutterDelete",
			"LineNr",
			"SignColumn",
		}

		for _, group in ipairs(light_bg) do
			vim.cmd.highlight(string.format("%s guibg=%s", group, lighter))
		end

		local dark_bg = {
			"NeoTreeNormal",
			"NeoTreeNormalNC",
			"NormalFloat",
			"Trouble",
		}

		for _, group in ipairs(dark_bg) do
			vim.cmd.highlight(string.format("%s guibg=%s", group, darker))
		end

		vim.cmd.highlight(string.format("VertSplit guifg=%s", lighter))
		vim.cmd.highlight("MatchParen guibg=NONE gui=underline")
	end,
}
