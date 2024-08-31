-- ===================================================================
-- ⌨ Keyboard Mappings
-- ===================================================================

local util = require("util")

-- Set leader keys
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Diagnostics
vim.keymap.set("n", "<LocalLeader>d", vim.diagnostic.open_float, { desc = "Show diagnostics" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })

-- Window navigation
vim.keymap.set("n", "<Left>", "<C-w>h")
vim.keymap.set("n", "<Down>", "<C-w>j")
vim.keymap.set("n", "<Up>", "<C-w>k")
vim.keymap.set("n", "<Right>", "<C-w>l")

-- UI toggles
vim.keymap.set("n", "<Leader>un", function()
  util.toggle_opt("number")
end, { desc = "Line numbers" })
vim.keymap.set("n", "<Leader>uw", function()
  util.toggle_opt("list")
end, { desc = "Whitespace" })
vim.keymap.set("n", "<Leader>uc", function()
  util.toggle_opt("cursorline")
end, { desc = "Cursorline" })

-- Miscellaneous
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal-mode" })
vim.keymap.set("n", "<Leader>q", "<Cmd>q<CR>", { desc = "Quit" })
vim.keymap.set("n", "<Esc>", "<Cmd>nohlsearch<CR>")
