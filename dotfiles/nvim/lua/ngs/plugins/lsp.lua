return {
  "neovim/nvim-lspconfig",

  dependencies = {
    { "folke/neodev.nvim", config = true },
    { "williamboman/mason.nvim", config = true },
    {
      "williamboman/mason-lspconfig.nvim",
      opts = {
        automatic_installation = { exclude = { "rnix" } },
      },
    },
    "WhoIsSethDaniel/mason-tool-installer.nvim",
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
      local pick = require("ngs.util").pick
      local telescope = require("telescope.builtin")

      local mappings = {
        ["<LocalLeader>"] = {
          name = "lsp",
          S = { telescope.lsp_workspace_symbols, "Workspace symbols" },
          a = { vim.lsp.buf.code_action, "Actions" },
          f = {
            function()
              vim.lsp.buf.format({ async = true })
            end,
            "Format",
          },
          i = { telescope.lsp_implementations, "Implementations" },
          n = { vim.lsp.buf.rename, "Rename" },
          r = { telescope.lsp_references, "References" },
          s = { pick("lsp_document_symbols", "get_dropdown", { previewer = false }), "Document symbols" },
        },
        K = { vim.lsp.buf.hover, "Hover" },
        ["[d"] = { vim.lsp.diagnostic.goto_prev, "Prev diagnostic" },
        ["]d"] = { vim.lsp.diagnostic.goto_next, "Next diagnostic" },
      }

      require("which-key").register(mappings, { buffer = buffer })

      local client = vim.lsp.get_client_by_id(event.data.client_id)
      if client and client.server_capabilities.documentHighlightProvider then
        vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
          buffer = event.buf,
          callback = vim.lsp.buf.document_highlight,
        })

        vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
          buffer = event.buf,
          callback = vim.lsp.buf.clear_references,
        })
      end
    end

    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
      callback = function(event)
        on_attach(event)
      end,
    })
  end,
}
