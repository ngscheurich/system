return {
	"folke/snacks.nvim",

	priority = 1000,
	lazy = false,

	---@type snacks.Config
	opts = {
		bigfile = { enabled = true },
		explorer = { enabled = true },
		gitbrowse = { enabled = true },
		image = { enabled = true },
		indent = { enabled = false, only_scope = true, only_current = true },
		notifier = { enabled = true },
		picker = { enabled = true },
		quickfile = { enabled = true },
		statuscolumn = { enabled = true },
	},

	keys = {
    -- stylua: ignore start
    -- Picker Quick
    { "<C-f>", function () Snacks.picker.files({layout = "ivy"}) end, desc = "Files" },
    { "<C-g>", function () Snacks.picker.grep({layout = "ivy"}) end, desc = "Grep" },
    { "<C-_>", function () Snacks.picker.lines() end, desc = "Lines" },
    { "<C-Space>", function () Snacks.picker.buffers({layout = "select"}) end, desc = "Buffers" },

    -- Picker General
    { "<Leader><Leader>", function() Snacks.picker.smart({layout = "ivy"}) end, desc = "Find files (smart)" },
    { "<Leader>r", function() Snacks.picker.resume() end, desc = "Resume picker" },

    -- Picker Find
    { "<Leader>ff", function() Snacks.picker.files() end, desc = "Files" },
    { "<Leader>fg", function() Snacks.picker.git_files() end, desc = "Git files" },
    { "<Leader>fp", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<Leader>fr", function() Snacks.picker.recent() end, desc = "Recent files" },

    -- Picker Search
    { "<Leader>sa", function() Snacks.picker.autocmds() end, desc = "Autocommands" },
    { "<Leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep buffers" },
    { "<Leader>s:", function() Snacks.picker.command_history() end, desc = "Command history" },
    { "<Leader>sc", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<Leader>sD", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
    { "<Leader>sd", function() Snacks.picker.diagnostics_buffer() end, desc = "Diagnostics (buffer)" },
    { "<Leader>sh", function() Snacks.picker.help() end, desc = "Help pages" },
    { "<Leader>sH", function() Snacks.picker.highlights() end, desc = "Highlights" },
    { "<Leader>si", function() Snacks.picker.icons() end, desc = "Icons" },
    { "<Leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
    { "<Leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<Leader>sl", function() Snacks.picker.loclist() end, desc = "Location list" },
    { "<Leader>sM", function() Snacks.picker.man() end, desc = "Man pages" },
    { "<Leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
    { "<Leader>sn", function() Snacks.picker.notifications() end, desc = "Notification history" },
    { "<Leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix list" },
    { "<Leader>s\"", function() Snacks.picker.registers() end, desc = "Registers" },
    { "<Leader>s/", function() Snacks.picker.search_history() end, desc = "Search history" },
    { "<Leader>su", function() Snacks.picker.undo() end, desc = "Undo history" },
    { "<Leader>sw", function() Snacks.picker.grep_word() end, desc = "Words" },

    -- Miscellaneous
		{ "<Leader>e", function() Snacks.explorer.open() end, desc = "Explorer (toggle)", },

		{ "<Leader>gB", function() Snacks.gitbrowse() end, desc = "View on GitHub", },

		{
			"<Leader>ui",
			function()
				if Snacks.indent.enabled then
					Snacks.indent.disable()
				else
					Snacks.indent.enable()
				end
			end,
			desc = "Indent Guides",
		},
		-- stylua: ignore end
	},
}
