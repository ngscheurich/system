-- ===================================================================
--  Language Server Client Setup
-- ===================================================================

-- =================================================================
--  mason.nvim
-- -----------------------------------------------------------------
--  https://github.com/williamboman/mason.nvim
--  Install/manage LSP servers, DAP servers, linters, formatters
-- -----------------------------------------------------------------
local mason_spec = { "williamboman/mason.nvim", config = true }

-- ===================================================================
--  neodev.nvim
-- -------------------------------------------------------------------
--  https://github.com/folke/neodev.nvim
--  LSP config for Neovim configuration and plugin development
-- -------------------------------------------------------------------
local neodev_spec = { "folke/neodev.nvim", config = true }

return {
  -- =================================================================
  --  nvim-lspconfig
  -- -----------------------------------------------------------------
  --  https://github.com/neovim/nvim-lspconfig
  --  Configuration presets for the built-in LSP client
  -- -----------------------------------------------------------------
  "neovim/nvim-lspconfig",

  dependencies = {
    mason_spec,
    neodev_spec,

    -- Bridge mason.nvim and nvim-lspconfig
    {
      "williamboman/mason-lspconfig.nvim",
      opts = {
        automatic_installation = { exclude = { "rnix" } },
      },
    },

    "folke/which-key.nvim",
    "hrsh7th/cmp-nvim-lsp",
    "nvim-telescope/telescope.nvim",
  },

  opts = {
    servers = {},
  },

  config = function(_, opts)
    local lspconfig = require("lspconfig")

    local capabilities = vim.lsp.protocol.make_client_capabilities()
    local nvim_cmp_capabilities = require("cmp_nvim_lsp").default_capabilities()
    capabilities = vim.tbl_deep_extend("force", capabilities, nvim_cmp_capabilities)

    for server, config in pairs(opts.servers) do
      lspconfig[server].setup(vim.tbl_extend("keep", config, {
        capabilities = capabilities,
      }))
    end

    local function on_attach(event)
      local buffer = event.buffer
      local pick = require("util").pick
      local telescope = require("telescope.builtin")

      require("which-key").add({
        { "<LocalLeader>", group = "lsp" },
        { "<LocalLeader>S", telescope.lsp_workspace_symbols, desc = "Workspace symbols" },
        { "<LocalLeader>a", vim.lsp.buf.code_action, desc = "Actions" },
        {
          "<LocalLeader>f",
          function()
            vim.lsp.buf.format({ async = true })
          end,
          desc = "Format",
        },
        { "<LocalLeader>i", telescope.lsp_implementations, desc = "Implementations" },
        { "<LocalLeader>n", vim.lsp.buf.rename, desc = "Rename" },
        { "<LocalLeader>r", telescope.lsp_references, desc = "References" },
        {
          "<LocalLeader>s",
          pick("lsp_document_symbols", "get_dropdown", { previewer = false }),
          desc = "Document symbols",
        },
        { "K", vim.lsp.buf.hover, desc = "Hover" },
      }, { buffer = buffer })
    end

    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
      callback = function(event)
        on_attach(event)
      end,
    })
  end,
}
