(local {:set map} vim.keymap)

(tset vim.g :mapleader " ")
(tset vim.g :maplocalleader ",")

(map [:n] "<Left>"  "<C-w>h")
(map [:n] "<Down>"  "<C-w>j")
(map [:n] "<Up>"    "<C-w>k")
(map [:n] "<Right>" "<C-w>l")

(map [:n] "<Esc>" "<Cmd>nohlsearch<CR>")
