return {
	-- Expanded matching delimiters
	"andymass/vim-matchup",
	-- Directory buffers
	"justinmk/vim-dirvish",
	-- Improved substitution
	"tpope/vim-abolish",
	-- Commands for common Unix programs
	"tpope/vim-eunuch",
	-- Project management
	"tpope/vim-projectionist",
	-- Expanded repeat
	"tpope/vim-repeat",
	-- Readline bindings
	"tpope/vim-rsi",
	-- Paired bracket mappings
	"tpope/vim-unimpaired",
	-- Send text to terminal multiplexer
	{
		"jpalardy/vim-slime",
		config = function()
			vim.g.slime_target = "zellij"
		end,
	},
}
