(local lsp vim.lsp)

;; On-attach function
(fn on-attach [client buffer]
  (local {: opt_local
          :api {:nvim_create_autocmd autocmd}
          :keymap {:set map}} vim)

  (local telescope (require :telescope.builtin))

  (local opts {:buffer buffer})

  (map [:n] "K"               lsp.buf.hover opts) 
  (map [:n] "<C-]>"           lsp.buf.definition opts)
  (map [:n] "[d"              lsp.diagnostic.goto_prev opts)
  (map [:n] "]d"              lsp.diagnostic.goto_next opts)

  (map [:n] "<LocalLeader>a"  lsp.buf.code_action opts)
  (map [:n] "<LocalLeader>R"  lsp.buf.rename opts)
  (map [:n]  "<LocalLeader>D" lsp.buf.declaration opts)
  (map [:n]  "<LocalLeader>n" lsp.buf.definition opts)
  (map [:n]  "<LocalLeader>d" vim.diagnostic.open_float opts)
  (map [:n]  "<LocalLeader>F" lsp.buf.format opts)
  (map [:n]  "<LocalLeader>t" lsp.buf.type_definition opts)

  (map [:n]  "<LocalLeader>i" telescope.lsp_implementations opts)
  (map [:n]  "<LocalLeader>r" telescope.lsp_references opts)
  (map [:n]  "<LocalLeader>s" telescope.lsp_document_symbols opts)
  (map [:n]  "<LocalLeader>S" telescope.lsp_workspace_symbols opts)

  (tset opt_local :foldmethd "expr")
  (tset opt_local :foldexpr vim.fn.nvim_treesitter#foldexpr)

  (when (client.supports_method "textDocument/formatting")
    (autocmd "BufWritePre" {:buffer buffer
                            :callback #(lsp.buf.format)})))

;; Capabilities
(local capabilities (let [cmp (require :cmp_nvim_lsp)]
                      (cmp.default_capabilities
                        (lsp.protocol.make_client_capabilities))))

;; Global lspconfig options
(local global-options {:on_attach on-attach
                       : capabilities})

;; Set up language servers
(local servers (require :lspconfig))

(servers.bashls.setup global-options)
(servers.cssls.setup global-options)
(servers.pyright.setup global-options)
(servers.rust_analyzer.setup global-options)
(servers.rnix.setup global-options)
(servers.terraformls.setup global-options)
(servers.tsserver.setup global-options)
(servers.vimls.setup global-options)

;; Use vim.tbl_extend?
(let [options global-options]
  (tset options :cmd ["elixir-ls"])
  (servers.elixirls.setup options)) 

(let [neodev (require :neodev)]
  (neodev.setup {})
  (servers.sumneko_lua.setup global-options))

;; Set up null-ls
(local null-ls (require :null-ls))
(local {:builtins {: formatting
                   : diagnostics }} null-ls)
(null-ls.setup {:sources [diagnostics.eslint_d
                          diagnostics.stylelint
                          formatting.eslint_d
                          formatting.prettierd
                          formatting.stylelint]})
