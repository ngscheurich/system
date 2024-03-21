-- ===================================================================
--  Text Editing Affordances
-- ===================================================================

return {
  -- =================================================================
  --  Leap
  -- -----------------------------------------------------------------
  --  https://github.com/ggandor/leap.nvim
  --  Intuitively jump around buffer
  -- -----------------------------------------------------------------
  {
    "ggandor/leap.nvim",
    dependencies = {
      -- Enhance f/t motions with Leap
      { "ggandor/flit.nvim", config = true },
    },
    config = function()
      require("leap").add_default_mappings()
    end,
  },

  -- =================================================================
  --  mini.surround
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-surround.md
  --  Edit surrounding pairs
  -- -----------------------------------------------------------------
  {
    "echasnovski/mini.surround",
    version = "*",
    opts = {
      mappings = {
        add = "<C-s>a",
        delete = "<C-s>d",
        find = "<C-s>f",
        find_left = "<C-s>F",
        highlight = "<C-s>h",
        replace = "<C-s>r",
        update_n_lines = "<C-s>n",
      },
    },
  },

  -- =================================================================
  --  mini.comment
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-comment.md
  --  Easily comment/uncomment lines
  -- -----------------------------------------------------------------
  { "echasnovski/mini.comment", version = "*", config = true },

  -- =================================================================
  --  mini.pairs
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-pairs.md
  --  Automatically insert pairs
  -- -----------------------------------------------------------------
  { "echasnovski/mini.pairs", version = "*", config = true },

  -- =================================================================
  --  mini.splitjoin
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-splitjoin.md
  --  Split and join arguments
  -- -----------------------------------------------------------------
  { "echasnovski/mini.splitjoin", version = "*", config = true },

  -- =================================================================
  --  mini.ai
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-ai.md
  --  Additional a/i text objects
  -- -----------------------------------------------------------------
  { "echasnovski/mini.ai", version = "*", config = true },

  -- =================================================================
  --  mini.align
  -- -----------------------------------------------------------------
  --  https://github.com/echasnovski/mini.nvim/blob/main/readmes/mini-align.md
  --  Interactively align text
  -- -----------------------------------------------------------------
  { "echasnovski/mini.align", version = "*", config = true },

  -- =================================================================
  --  conform.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/stevearc/conform.nvim
  --  Automatically format text
  -- -----------------------------------------------------------------
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
