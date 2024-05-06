-- ===================================================================
--  Note-Taking Tools
-- ===================================================================

return {
  -- =================================================================
  --  obsidian.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/epwalsh/obsidian.nvim
  --  Navigate and edit Obsidian notes
  -- -----------------------------------------------------------------
  {
    "epwalsh/obsidian.nvim",
    version = "*",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      workspaces = {
        {
          name = "Notes",
          path = vim.fn.expand("~") .. "/Notes",
        },
      },
      daily_notes = {
        folder = vim.fn.expand("~") .. "/Notes/Daily\\ Notes",
      },
    },
  },
}
