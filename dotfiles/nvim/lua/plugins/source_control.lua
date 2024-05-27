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

      local mappings = {
        ["<Leader>g"] = {
          b = { gitsigns.toggle_current_line_blame, "Line blame (toggle)" },
          d = { gitsigns.toggle_deleted, "Deleted (toggle)" },
          h = { gitsigns.toggle_linehl, "Line highlight (toggle)" },
          p = { gitsigns.preview_hunk, "Preview hunk" },
          r = { gitsigns.reset_hunk, "Reset hunk" },
        },

        ["]h"] = { gitsigns.next_hunk, "Next hunk" },
        ["[h"] = { gitsigns.prev_hunk, "Previous hunk" },
      }

      require("which-key").register(mappings)
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

      vim.keymap.set("n", "<Leader>gs", neogit.open, { desc = "Status" })
      vim.keymap.set("n", "<Leader>gc", function()
        neogit.open({ "commit" })
      end, { desc = "Commit" })
    end,
  },
}
