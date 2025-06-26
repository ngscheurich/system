-- ===================================================================
--  Neotest
-- -------------------------------------------------------------------
--  https://github.com/nvim-neotest/neotest
--  Extensible framework for interacting with tests
--  testing
-- -------------------------------------------------------------------
return {
	"nvim-neotest/neotest",

	dependencies = {
		"nvim-neotest/nvim-nio",
		"antoinemadec/FixCursorHold.nvim",
		"jfpedroza/neotest-elixir",
	},

	config = function()
		require("neotest").setup({
			adapters = { require("neotest-elixir") },
		})
	end,

	keys = {
    -- stylua: ignore start
    {"<Leader>tn", function () require("neotest").run.run() end, desc = "Nearest"},
    {"<Leader>tt", function () require("neotest").run.run_last() end, desc = "Last"},
    {"<Leader>tf", function () require("neotest").run.run(vim.fn.expand("%")) end, desc = "File"},
    {"<Leader>ts", function () require("neotest").summary.toggle() end, desc = "Summary (toggle)"},
		-- stylue: ignore end
	},
}
