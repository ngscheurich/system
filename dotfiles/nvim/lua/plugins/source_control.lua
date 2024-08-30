-- ===================================================================
--  Source Control Tools
-- ===================================================================

return {

  -- =================================================================
  --  gitsigns.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/lewis6991/gitsigns.nvim
  --  Git gutter indicators, inline blame, and more
  -- -----------------------------------------------------------------
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

      require("which-key").add({
        { "<Leader>gb", gitsigns.toggle_current_line_blame, desc = "Line blame (toggle)" },
        { "<Leader>gd", gitsigns.toggle_deleted, desc = "Deleted (toggle)" },
        { "<Leader>gh", gitsigns.toggle_linehl, desc = "Line highlight (toggle)" },
        { "<Leader>gp", gitsigns.preview_hunk, desc = "Preview hunk" },
        { "<Leader>gr", gitsigns.reset_hunk, desc = "Reset hunk" },
        { "[h", gitsigns.prev_hunk, desc = "Previous hunk" },
        { "]h", gitsigns.next_hunk, desc = "Next hunk" },
      })
    end,
  },

  -- =================================================================
  --  NeoGit
  -- -----------------------------------------------------------------
  --  https://github.com/NeogitOrg/neogit
  --  Git interface inspired by Magit
  -- -----------------------------------------------------------------
  {
    "NeogitOrg/neogit",
    branch = "master",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      local neogit = require("neogit")

      neogit.setup({
        disable_hint = true,
        signs = {
          hunk = { "", "" },
          item = { "󰅂", "󰅀" },
          section = { "󰅂", "󰅀" },
        },
      })

      require("which-key").add({
        { "<Leader>gb", neogit.open, desc = "Status" },
        {
          "<Leader>gc",
          function()
            neogit.open({ "commit" })
          end,
          desc = "Commit",
        },
      })
    end,
  },
}
