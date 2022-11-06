(local {:nvim_create_augroup augroup
        :nvim_create_autocmd autocmd} vim.api)

(let [group (augroup "yank-highlight" {})]
     (autocmd "TextYankPost" {:pattern "*"
                              :callback #(vim.highlight.on_yank)}))
