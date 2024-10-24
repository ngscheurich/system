-- ===================================================================
--  Completion Systems
-- ===================================================================

-- ===================================================================
--  lspkind-nvim
-- -------------------------------------------------------------------
--  https://github.com/onsails/lspkind.nvim
--  Pictograms for completion candidates
-- -------------------------------------------------------------------
local lspkind_spec = "onsails/lspkind.nvim"

return {
  -- =================================================================
  --  nvim-cmp
  -- -----------------------------------------------------------------
  --  https://github.com/hrsh7th/nvim-cmp
  --  Multi-source completion engine
  -- -----------------------------------------------------------------
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",

    dependencies = {
      lspkind_spec,
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
    },

    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local lspkind = require("lspkind")

      local opts = {
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },

        mapping = cmp.mapping.preset.insert({
          ["<C-Space>"] = cmp.mapping.complete({}),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<C-E>"] = cmp.mapping.close(),

          ["<C-F>"] = cmp.mapping.scroll_docs(4),
          ["<C-B>"] = cmp.mapping.scroll_docs(-4),

          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { "i", "s" }),

          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        }),

        sources = cmp.config.sources({
          { name = "luasnip" },
          { name = "nvim_lsp" },
          { name = "path" },
          { name = "buffer", option = { keyword_length = 4 } },
        }),

        formatting = {
          format = lspkind.cmp_format({
            before = require("tailwind-tools.cmp").lspkind_format,
          }),
        },
      }

      cmp.setup(opts)
      cmp.setup.filetype({ "sql" }, {
        sources = {
          { name = "vim-dadbod-completion" },
          { name = "buffer" },
        },
      })
    end,
  },
}
