return {
  -- Intuitively jump around buffer
  {
    "ggandor/leap.nvim",
    dependencies = {
      { "ggandor/flit.nvim", config = true },
    },
    config = function()
      require("leap").add_default_mappings()
    end,
  },

  -- Edit surrounding pairs
  { "echasnovski/mini.surround", version = "*", config = true },

  -- Easily comment/uncomment
  { "echasnovski/mini.comment", version = "*", config = true },

  -- Automatically insert pairs
  { "echasnovski/mini.pairs", version = "*", config = true },

  -- Split and join arguments
  { "echasnovski/mini.splitjoin", version = "*", config = true },

  -- Additional a/i text objects
  { "echasnovski/mini.ai", version = "*", config = true },

  -- Align text interactively
  { "echasnovski/mini.align", version = "*", config = true },

  -- Move selected text
  { "echasnovski/mini.move", version = "*", config = true },

  -- Format text
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {},
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
    },
  },
}
