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
}
