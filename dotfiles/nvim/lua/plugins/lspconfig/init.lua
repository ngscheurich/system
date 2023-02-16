local M = { "neovim/nvim-lspconfig" }

M.dependencies = {
  "folke/neodev.nvim",
  "jose-elias-alvarez/null-ls.nvim",
  "smjonas/inc-rename.nvim",
  "williamboman/mason-lspconfig.nvim",
  "williamboman/mason.nvim",
}

function M.config()
  local servers = {
    bashls = {},
    clangd = {},
    elixirls = {},
    rust_analyzer = {},
    solargraph = {},
    sumneko_lua = {
      Lua = {
        telemetry = { enable = false },
        workspace = { checkThirdParty = false },
      },
    },
    tailwindcss = {},
    tsserver = {},
  }

  require("neodev").setup({})

  require("mason").setup({
    PATH = "prepend",
  })

  local function on_attach(client, buffer)
    require("nvim-navic").attach(client, buffer)

    require("plugins.lspconfig.keymaps").setup(client, buffer)
    require("plugins.lspconfig.formatting").setup(client, buffer)

    vim.wo.foldmethod = "expr"
    vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
    vim.wo.foldlevel = 99
  end

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  local mason_lspconfig = require("mason-lspconfig")

  mason_lspconfig.setup({
    ensure_installed = vim.tbl_keys(servers),
  })

  mason_lspconfig.setup_handlers({
    function(server)
      require("lspconfig")[server].setup({
        capabilities = capabilities,
        on_attach = on_attach,
        settings = servers[server],
      })
    end,
  })

  local null_ls = require("null-ls")

  null_ls.setup({
    sources = {
      null_ls.builtins.diagnostics.eslint,
      null_ls.builtins.diagnostics.stylelint,

      null_ls.builtins.formatting.prettier,
      null_ls.builtins.formatting.stylelint,
      null_ls.builtins.formatting.stylua,
    },
    on_attach = on_attach,
  })
end

return M
