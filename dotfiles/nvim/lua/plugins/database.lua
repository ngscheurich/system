-- ===================================================================
--  Database Tools
-- ===================================================================

return {
  -- =================================================================
  --  vim-dadbod-ui
  -- -----------------------------------------------------------------
  --  https://github.com/kristijanhusak/vim-dadbod-ui
  --  UI for working with relational databases
  -- -----------------------------------------------------------------
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      { "tpope/vim-dadbod" },
      { "kristijanhusak/vim-dadbod-completion", ft = { "sql", "mysql", "plsql" } },
    },
    cmd = {
      "DBUI",
      "DBUIToggle",
      "DBUIAddConnection",
      "DBUIFindBuffer",
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts = 1
    end,
  },
}
