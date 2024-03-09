-- ===================================================================
--  Source Control Tools
-- ===================================================================

return {
  {
    "lewis6991/gitsigns.nvim",
    dependencies = { "folke/which-key.nvim" },
    config = function()
      local gitsigns = require("gitsigns")

      gitsigns.setup({
        signs = {
          add = { text = "┃" },
          change = { text = "┃" },
          changedelete = { text = "┃" },
          delete = { text = "┃" },
          topdelete = { text = "┃" },
          untracked = { text = "┇" },
        },
      })

      local mappings = {
        ["<Leader>g"] = {
          name = "Git",

          b = { gitsigns.toggle_current_line_blame, "Line blame (toggle)" },
          d = { gitsigns.toggle_deleted, "Deleted (toggle)" },
          h = { gitsigns.toggle_linehl, "Line highlight (toggle)" },
          p = { gitsigns.preview_hunk, "Preview hunk" },
          r = { gitsigns.reset_hunk, "Reset hunk" },
        },

        ["]c"] = { gitsigns.next_hunk, "Next hunk" },
        ["[c"] = { gitsigns.prev_hunk, "Previous hunk" },
      }

      require("which-key").register(mappings)
    end,
  },

  "tpope/vim-fugitive",
}
