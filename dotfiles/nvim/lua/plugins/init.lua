return {
  -- Interface
  {
    "RRethy/vim-illuminate",
    keys = {
      { "<Leader>ti", "<Cmd>IlluminateToggle<CR>", desc = "Illuminate" },
    },
  },
  "rcarriga/nvim-notify",
  { "nvim-tree/nvim-web-devicons", config = true },
  { "yamatsum/nvim-nonicons", config = true },
  { "j-hui/fidget.nvim", config = true },

  -- Code Intelligence
  { "smjonas/inc-rename.nvim", config = true },

  -- Navigation
  "andymass/vim-matchup",
  "justinmk/vim-dirvish",

  -- Syntax
  "fladson/vim-kitty",
  "sheerun/vim-polyglot",

  -- Source Control
  "tpope/vim-fugitive",

  -- Miscellaneous
  "tpope/vim-abolish",
  "tpope/vim-eunuch",
  "tpope/vim-projectionist",
  "tpope/vim-repeat",
  "tpope/vim-rsi",
  "tpope/vim-unimpaired",
}
