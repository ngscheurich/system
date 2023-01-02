local M = { "neovim/nvim-lspconfig" }

M.dependencies = {
  "folke/neodev.nvim",
  "williamboman/mason-lspconfig.nvim",
  "williamboman/mason.nvim",
}

function M.config()
  local servers = {
    bashls = {},
    elixirls = {},
    sumneko_lua = {
      Lua = {
        telemetry = { enable = false },
        workspace = { checkThirdParty = false },
      },
    },
    tsserver = {},
  }

  require("neodev").setup({})
  require("mason").setup()

  local function on_attach(client, buffer)
    require("plugins.lsp.keymaps").setup(client, buffer)
    require("plugins.lsp.formatting").setup(client, buffer)

    vim.opt_local.foldmethod = "expr"
    vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
    vim.opt.foldlevel = 99
  end

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  local mason_lspconfig = require("mason-lspconfig")

  mason_lspconfig.setup({
    ensure_installed = vim.tbl_keys(servers),
  })

  mason_lspconfig.setup_handlers {
    function(server)
      require("lspconfig")[server].setup {
        capabilities = capabilities,
        on_attach = on_attach,
        settings = servers[server],
      }
    end,
  }
end

return M
