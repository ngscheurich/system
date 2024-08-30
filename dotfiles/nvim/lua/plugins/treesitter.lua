-- ===================================================================
--  Tree-sitter
-- ===================================================================

return {
  -- =================================================================
  --  nvim-treesitter
  -- -----------------------------------------------------------------
  --  https://github.com/nvim-treesitter/nvim-treesitter
  --  Tree-sitter configurations and abstraction layer
  -- -----------------------------------------------------------------
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      highlight = { enable = true },
      indent = { enable = true },
    },
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
  },

  -- =================================================================
  --  nvim-treesitter-textobjects
  -- -----------------------------------------------------------------
  --  https://github.com/nvim-treesitter/nvim-treesitter-textobjects
  --  Syntax-aware text-objects, movements, and more
  -- -----------------------------------------------------------------
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require("nvim-treesitter.configs").setup({
        textobjects = {
          select = {
            enable = true,
            keymaps = {
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ib"] = "@block.inner",
              ["ab"] = "@block.outer",
            },
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              ["]c"] = "@class.outer",
              ["]]"] = "@function.outer",
              ["]b"] = "@block.outer",
            },
            goto_next_end = {
              ["]C"] = "@class.outer",
              ["]}"] = "@function.outer",
              ["]B"] = "@block.outer",
            },
            goto_previous_start = {
              ["[c"] = "@class.outer",
              ["[["] = "@function.outer",
              ["]b"] = "@block.outer",
            },
            goto_previous_end = {
              ["[C"] = "@class.outer",
              ["[{"] = "@function.outer",
              ["[B"] = "@block.outer",
            },
          },
        },
      })
    end,
  },
}
