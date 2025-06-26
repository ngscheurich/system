-- ===================================================================
--  obsidian.nvim
-- -------------------------------------------------------------------
--  https://github.com/obsidian-nvim/obsidian.nvim
--  Write in and navigate Obsidian vaults
--  notes
-- -------------------------------------------------------------------
return {
  "obsidian-nvim/obsidian.nvim",
  version = "*",
  -- lazy = true,
  -- ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  -- event = {
  --   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
  --   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
  --   -- refer to `:h file-pattern` for more examples
  --   "BufReadPre path/to/my-vault/*.md",
  --   "BufNewFile path/to/my-vault/*.md",
  -- },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "OXY2DEV/markview.nvim",
  },
  opts = {
    workspaces = {
      {
        name = "Notes",
        path = "~/Vaults/Notes",
      },
    },

    daily_notes = {
      folder = "journal",
    },

    completion = {
      nvim_cmp = false,
      blink = true,
    },
  },
}
