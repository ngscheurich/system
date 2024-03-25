-- ===================================================================
--  Plugins
-- ===================================================================

return {
  -- =================================================================
  --  lazy.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/folke/lazy.nvim
  --  Modern plugin manager
  -- -----------------------------------------------------------------
  {
    "folke/lazy.nvim",
    version = "*",
    keys = { { "<Leader>P", "<Cmd>Lazy<CR>", desc = "Nearest" } },
  },

  -- Evaluating
  {
    "glacambre/firenvim",
    -- Lazy load firenvim
    -- Explanation: https://github.com/folke/lazy.nvim/discussions/463#discussioncomment-4819297
    lazy = not vim.g.started_by_firenvim,
    build = function()
      vim.fn["firenvim#install"](0)
    end,
    init = function()
      vim.api.nvim_create_augroup("firenvim", {})
      vim.api.nvim_create_autocmd({ "BufEnter" }, {
        group = "firenvim",
        pattern = "github.com_*.txt",
        command = "set filetype=markdown",
      })
      -- vim.api.nvim_create_autocmd({ "BufEnter" }, {
      --   group = "firenvim",
      --   pattern = "app.shortcut.com_*.txt",
      --   command = "set filetype=markdown",
      -- })
    end,
  },
}
