(local {:set map} vim.keymap)

(set vim.g.test#strategy {:nearest :neovim
                          :file :neovim
                          :suite :basic})

(map [:n] "<Leader>tf" "<Cmd>TestFile<CR>")
(map [:n] "<Leader>tl" "<Cmd>TestLast<CR>")
(map [:n] "<Leader>tn" "<Cmd>TestNearest<CR>")
(map [:n] "<Leader>ts" "<Cmd>TestSuite<CR>")
(map [:n] "<Leader>tv" "<Cmd>TestVisit<CR>")
