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
    lazy = true,
    ft = "markdown",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = {
      "BufReadPre " .. vim.fn.expand("~") .. "/Notes/**.md",
      "BufNewFile " .. vim.fn.expand("~") .. "/Notes/**.md",
    },
    opts = {
      workspaces = {
        {
          name = "Notes",
          -- TODO: Expand path
          path = "~/Notes",
        },
      },
    },
  },
}
