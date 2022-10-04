;; On-attach function
(fn on-attach [client bufnr]
  (import-macros {: buf-map!} :themis.keybind)
  (import-macros {: augroup!
                  : clear!
                  : autocmd!} :themis.event)

  (local telescope (require :telescope.builtin))

  (buf-map! [n] "K" vim.lsp.buf.hover)
  (buf-map! [nv] "<LocalLeader>a" vim.lsp.buf.code_action)
  (buf-map! [nv] "<LocalLeader>rn" vim.lsp.buf.rename)
  (buf-map! [n] "<LocalLeader>gD" vim.lsp.buf.declaration)
  (buf-map! [n] "<LocalLeader>gd" vim.lsp.buf.definition)
  (buf-map! [n] "<LocalLeader>gt" vim.lsp.buf.type_definition)
  (buf-map! [n] "<LocalLeader>li" telescope.lsp_implementations)
  (buf-map! [n] "<LocalLeader>lr" telescope.lsp_references)
  (buf-map! [n] "<LocalLeader>ls" telescope.lsp_document_symbols)
  (buf-map! [n] "<LocalLeader>lS" telescope.lsp_workspace_symbols)

  (when (client.supports_method "textDocument/formatting")
    (augroup! lsp-format-before-saving
      (clear! {:buffer bufnr})
      (autocmd! BufWritePre <buffer> vim.lsp.buf.format {:buffer bufnr}))))

;; Install language servers automatically
(local mason (require :mason-lspconfig))
(mason.setup {:automatic_installation true})

;; Global lsp-config options
(local global-options {:on_attach on-attach
                       : capabilities})

;; Set up language servers
(local config (require :lspconfig))

(config.bashls.setup global-options)
(config.cssls.setup global-options)
(config.elixirls.setup global-options)
(config.pyright.setup global-options)
(config.rls.setup global-options)
(config.rnix.setup global-options)
(config.terraformls.setup global-options)
(config.tsserver.setup global-options)
(config.vimls.setup global-options)

(let [lua-dev (require :lua-dev)]
  (lua-dev.setup {})
  (config.sumneko_lua.setup {:on_attach on-attach}))


;; Set up null-ls
(local null-ls (require :null-ls))
(local {:builtins {: formatting
                   : diagnostics }} null-ls)
(null-ls.setup {:sources [diagnostics.eslint_d
                          diagnostics.stylelint
                          formatting.eslint_d
                          formatting.prettierd
                          formatting.stylelint]})
                          ;; formatting.stylua]})
