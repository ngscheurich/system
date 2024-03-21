-- ===================================================================
-- ⌨ Keyboard Mappings
-- ===================================================================

local util = require("util")

-- Set leader keys
vim.g.mapleader = " "
vim.g.maplocalleader = ","

local set = vim.keymap.set

set("n", "<Leader>q", "<Cmd>q<CR>", { desc = "Quit" })
set("n", "<Leader>d", vim.diagnostic.open_float, { desc = "Show diagnostics" })
set("n", "<Esc>", "<Cmd>nohlsearch<CR>")

set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal-mode" })

-- Window navigation
set("n", "<Left>", "<C-w>h")
set("n", "<Down>", "<C-w>j")
set("n", "<Up>", "<C-w>k")
set("n", "<Right>", "<C-w>l")

-- UI toggles
set("n", "<Leader>un", function()
  util.toggle_opt("number")
end, { desc = "Line numbers" })
set("n", "<Leader>uw", function()
  util.toggle_opt("list")
end, { desc = "Whitespace" })
set("n", "<Leader>uc", function()
  util.toggle_opt("cursorline")
end, { desc = "Cursorline" })
