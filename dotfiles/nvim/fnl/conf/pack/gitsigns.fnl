(local {:set map} vim.keymap)
(local gitsigns (require :gitsigns))

(gitsigns.setup
  {:signs {:add          {:text :┃}
           :change       {:text :┃}
           :delete       {:text :┃}
           :topdelete    {:text :┃}
           :changedelete {:text :┃}}})

(map [:n] "]c" "<Cmd>Gitsigns next_hunk<CR>")
(map [:n] "[c" "<Cmd>Gitsigns prev_hunk<CR>")

(map [:n] "<Leader>gb" "<Cmd>Gitsigns toggle_current_line_blame<CR>")
(map [:n] "<Leader>gp" "<Cmd>Gitsigns preview_hunk<CR>")
(map [:n] "<Leader>gr" "<Cmd>Gitsigns reset_hunk<CR>")
