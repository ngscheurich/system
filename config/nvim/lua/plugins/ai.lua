-- ===================================================================
-- 󱚠 Artificial Intelligence, Or Something Like That
-- ===================================================================

return {
  -- =================================================================
  --  GitHub Copilot
  -- -----------------------------------------------------------------
  --  https://github.com/github/copilot.vim
  --  Official GitHub Copilot integration
  -- -----------------------------------------------------------------
  {
    "github/copilot.vim",
    config = function()
      vim.cmd("Copilot disable")

      vim.keymap.set("n", "<Leader>ai", function()
        local subcmd = "enable"
        if vim.fn["copilot#Enabled"]() == 1 then
          subcmd = "disable"
        end
        vim.cmd("Copilot " .. subcmd)
        vim.print("Copilot " .. subcmd .. "d")
      end, { desc = "Toggle Copilot" })

      vim.keymap.set("n", "<Leader>ap", "<Cmd>Copilot panel<CR>", { desc = "Copilot panel" })
      vim.keymap.set("n", "<Leader>as", "<Cmd>Copilot status<CR>", { desc = "Copilot status" })
      vim.keymap.set("i", "<M-O>", 'copilot#Accept("\\<CR>")', { expr = true, replace_keycodes = false })

      vim.g.copilot_no_tab_map = true
    end,
  },
}
