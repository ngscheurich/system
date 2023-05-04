local M = { "neovim/nvim-lspconfig" }

M.ft = {
  "bash",
  "c",
  "css",
  "elixir",
  "html",
  "javascript",
  "lua",
  "ruby",
  "rust",
  "typescript",
}

M.dependencies = {
  "folke/neodev.nvim",
  "jose-elias-alvarez/null-ls.nvim",
  "smjonas/inc-rename.nvim",
  "williamboman/mason-lspconfig.nvim",
  "williamboman/mason.nvim",
}

function M.config()
  require("neodev").setup({})
  require("mason").setup()

  local function on_attach(client, buffer)
    -- require("nvim-navic").attach(client, buffer)
    -- require("nvim-navbuddy").attach(client, buffer)

    require("plugins.lsp.keymaps").on_attach(client, buffer)
    require("plugins.lsp.formatting").on_attach(client, buffer)

    vim.wo.foldmethod = "expr"
    vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
    vim.wo.foldlevel = 99
  end

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

  local mason_lspconfig = require("mason-lspconfig")
  mason_lspconfig.setup()

  local servers = require("plugins.lsp.servers")
  mason_lspconfig.setup_handlers({
    function(server)
      if server ~= "rnix" then
        local conf = servers[server]
        local settings = {}
        if conf and conf.settings then
          settings = conf.settings
        end

        local spec = {
          capabilities = capabilities,
          on_attach = on_attach,
          settings = settings,
        }

        if conf ~= nil and conf.cmd ~= nil then
          spec.cmd = conf.cmd
        end

        require("lspconfig")[server].setup(spec)
      end
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
