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

  local colorschemes = {
    ["gruvbox-dark"] = "catppuccin-mocha",
    ["gruvbox-light"] = "catppuccin-latte",
  }

  vim.ui.select(themes, {
    prompt = "Select theme:",
  }, function(choice)
    io.popen(string.format("switch-theme %s", choice))
    vim.cmd.colorscheme(colorschemes[choice] or choice)
  end)
end, {})
