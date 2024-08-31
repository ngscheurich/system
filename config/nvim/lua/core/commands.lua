-- ===================================================================
-- ⌘ Autocommands/User Commands
-- ===================================================================

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local command = vim.api.nvim_create_user_command

local user_group = augroup("UserGroup", {})

autocmd({ "TextYankPost" }, {
  desc = "Highlights text on yank",
  group = user_group,
  pattern = "*",
  callback = function()
    vim.highlight.on_yank()
  end,
})

autocmd({ "VimResized" }, {
  desc = "Equalize window sizes",
  group = user_group,
  pattern = "*",
  callback = function()
    vim.cmd.wincmd("=")
  end,
})

command("SwitchTheme", function()
  -- TODO: Load theme plugin if not loaded
  local path = vim.fn.globpath("/etc/system/themes", "*")
  local theme_paths = vim.split(path, "\n")

  local themes = vim.tbl_map(function(value)
    return vim.fn.fnamemodify(value, ":t")
  end, theme_paths)

  vim.ui.select(themes, { prompt = "Select theme:" }, function(choice)
    io.popen(string.format("switch-theme %s", choice))
    vim.cmd.colorscheme(choice)
  end)
end, {})
