-- ===================================================================
-- ⌘ Autocommands/User Commands
-- ===================================================================

local autocmd = vim.api.nvim_create_autocmd

autocmd({ "TextYankPost" }, {
  desc = "Highlights text on yank",
  pattern = "*",
  callback = function()
    vim.highlight.on_yank()
  end,
})

vim.api.nvim_create_user_command("SwitchTheme", function()
  local theme_paths = vim.split(vim.fn.globpath("/etc/system/themes", "*"), "\n")

  local themes = vim.tbl_map(function(value)
    return vim.fn.fnamemodify(value, ":t")
  end, theme_paths)

  vim.ui.select(themes, {
    prompt = "Select theme:",
  }, function(choice)
    io.popen(string.format("switch-theme %s", choice))
    vim.cmd.colorscheme(choice)
  end)
end, {})
