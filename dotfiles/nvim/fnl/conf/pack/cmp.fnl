(local cmp (require :cmp))
(local luasnip (require :luasnip))

(local map cmp.mapping)

(local whitespace? (fn [line col]
                    (let [lines (vim.api.nvim_buf_get_lines 0 (- line 1) line true)
                          cur-line (. lines 1)
                          cur-char (line:sub col col)]
                         (cur-char:match :%s))))

(local words-before? (fn []
                      (let [(line col) (unpack (vim.api.nvim_win_get_cursor 0))]
                           (and (~= col 0)
                                (= (whitespace? line col) nil)))))

(cmp.setup {:snippet {:expand (fn [args] (luasnip.lsp_expand args.body))}
            :window  {:completion (cmp.config.window.bordered)
                      :documentation (cmp.config.window.bordered)}

            :mapping (cmp.mapping.preset.insert {:<C-b> (cmp.mapping.scroll_docs -4)
                                                 :<C-f> (cmp.mapping.scroll_docs 4)
                                                 :<C-Space> (cmp.mapping.complete)
                                                 :<C-e> (cmp.mapping.abort)
                                                 :<CR> (cmp.mapping.confirm)})

            :sources (cmp.config.sources [{:name :nvim_lsp}
                                          {:name :luasnip}
                                          {:name :conjure}]
                                         [{:name :buffer}])})
