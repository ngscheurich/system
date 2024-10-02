-- ===================================================================
--  Plugins
-- ===================================================================

return {
  -- =================================================================
  --  lazy.nvim
  -- -----------------------------------------------------------------
  --  https://github.com/folke/lazy.nvim
  --  Modern plugin manager
  -- -----------------------------------------------------------------
  {
    "folke/lazy.nvim",
    version = "*",
    keys = { { "<Leader>P", "<Cmd>Lazy<CR>", desc = "Nearest" } },
  },

  -- Evaluating
  {
    "glacambre/firenvim",
    -- Lazy load firenvim
    -- Explanation: https://github.com/folke/lazy.nvim/discussions/463#discussioncomment-4819297
    lazy = not vim.g.started_by_firenvim,
    build = function()
      vim.fn["firenvim#install"](0)
    end,
    init = function()
      vim.api.nvim_create_augroup("firenvim", {})
      vim.api.nvim_create_autocmd({ "BufEnter" }, {
        group = "firenvim",
        pattern = "github.com_*.txt",
        command = "set filetype=markdown",
      })
      -- vim.api.nvim_create_autocmd({ "BufEnter" }, {
      --   group = "firenvim",
      --   pattern = "app.shortcut.com_*.txt",
      --   command = "set filetype=markdown",
      -- })
    end,
  },

  {
    "stevearc/overseer.nvim",
    opts = {
      templates = {
        "builtin",
        "user.godot_run",
      },
    },
  },

  {
    "mistweaverco/kulala.nvim",
    config = function()
      local kulala = require("kulala")

      kulala.setup({

        icons = {
          inlay = {
            loading = "󰔟 ",
            done = " ",
            error = " ",
          },
        },
      })

      require("which-key").add({
        { "<Leader>h", group = "HTTP" },
        { "<Leader>hq", kulala.close, desc = "Close" },
        { "<Leader>hc", kulala.copy, desc = "Copy" },
        { "<Leader>hC", kulala.from_curl, desc = "From curl" },
        { "<Leader>he", kulala.get_selected_env, desc = "Get selected env" },
        { "<Leader>hi", kulala.inspect, desc = "Inspect" },
        { "<Leader>hn", kulala.jump_next, desc = "Next" },
        { "<Leader>hp", kulala.jump_prev, desc = "Prev" },
        { "<Leader>hl", kulala.replay, desc = "Replay" },
        { "<Leader>hr", kulala.run, desc = "Run" },
        { "<Leader>ha", kulala.run_all, desc = "Run all" },
        { "<Leader>hs", kulala.scratchpad, desc = "Scratchpad" },
        { "<Leader>h/", kulala.search, desc = "Search" },
        { "<Leader>h?", kulala.show_stats, desc = "Show stats" },
        { "<Leader>ht", kulala.toggle_view, desc = "Toggle view" },
      })
    end,
  },

  {
    "nvim-pack/nvim-spectre",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      replace_engine = {
        ["sed"] = {
          warn = false,
        },
      },
    },
  },

  {
    "luckasRanarison/tailwind-tools.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = true,
  },

  {
    "laytan/tailwind-sorter.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-lua/plenary.nvim" },
    build = "cd formatter && npm ci && npm run build",
    config = function()
      require("tailwind-sorter").setup({
        on_save_enabled = true, -- If `true`, automatically enables on save sorting.
        on_save_pattern = { "*.html", "*.js", "*.jsx", "*.tsx", "*.heex", "*.ex" }, -- The file patterns to watch and sort.
        node_path = "node",
      })
    end,
  },
}
