return {
  ----------------------------------------------------------
  -- Interface
  ----------------------------------------------------------
  "rcarriga/nvim-notify",
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons", config = true },
  { "j-hui/fidget.nvim", config = true },

  ----------------------------------------------------------
  -- Editing
  ----------------------------------------------------------
  { "smjonas/inc-rename.nvim", config = true },

  ----------------------------------------------------------
  -- Navigation
  ----------------------------------------------------------
  "andymass/vim-matchup",
  "justinmk/vim-dirvish",

  ----------------------------------------------------------
  -- Syntax
  ----------------------------------------------------------
  "fladson/vim-kitty",
  "imsnif/kdl.vim",
  "sheerun/vim-polyglot",

  ----------------------------------------------------------
  -- Source Control
  ----------------------------------------------------------
  "tpope/vim-fugitive",
  ----------------------------------------------------------

  ----------------------------------------------------------
  -- Miscellaneous
  ----------------------------------------------------------
  "tpope/vim-abolish",
  "tpope/vim-dispatch",
  "tpope/vim-eunuch",
  "tpope/vim-projectionist",
  "tpope/vim-repeat",
  "tpope/vim-rsi",
  "tpope/vim-unimpaired",

  {
    "mhanberg/output-panel.nvim",
    event = "VeryLazy",
    config = function()
      require("output_panel").setup()
    end,
  },

  {
    "catppuccin/nvim",
    name = "catppuccin",
    cond = _G.colorscheme == "catppuccin",
    config = function()
      vim.cmd("colorscheme " .. _G.colorscheme)
    end,
  },

  {
    "RRethy/nvim-base16",
    cond = _G.colorscheme == "base16-tomorrow-night",
    config = function()
      local apply_colors = function()
        local light_bg = {
          "CursorLineNr",
          "GitGutterAdd",
          "GitGutterChange",
          "GitGutterChangeDelete",
          "GitGutterDelete",
          "LineNr",
          "SignColumn",
        }
        for _, group in ipairs(light_bg) do
          vim.cmd(string.format("hi %s guibg=#212326", group))
        end

        local dark_bg = {
          "NeoTreeNormal",
          "NeoTreeNormalNC",
          "NormalFloat",
          "Trouble",
        }
        for _, group in ipairs(dark_bg) do
          vim.cmd(string.format("hi %s guibg=#1b1c1f", group))
        end

        -- vim.cmd("hi GitGutterAdd          guifg=#b5bd68")
        -- vim.cmd("hi GitGutterChange       guifg=#f0c574")
        -- vim.cmd("hi GitGutterDelete       guifg=#cc6666")
        -- vim.cmd("hi GitGutterChangeDelete guifg=#80a1bd")

        vim.cmd("hi TSVariable guifg=#c5c8c6")
        vim.cmd("hi MatchParen guibg=NONE gui=underline")
        vim.cmd("hi VertSplit guifg=#212326")
      end

      vim.api.nvim_create_autocmd({ "ColorScheme" }, {
        pattern = { colorscheme },
        callback = apply_colors,
      })

      vim.api.nvim_create_autocmd({ "FileType" }, {
        pattern = { "help" },
        callback = function()
          vim.opt_local.signcolumn = "no"
        end,
      })

      vim.cmd("colorscheme " .. _G.colorscheme)
      -- vim.cmd("highlight VertSplit guifg=#363a41")
    end,
  },
}
