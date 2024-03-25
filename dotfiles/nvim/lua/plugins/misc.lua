-- ===================================================================
--  A Collected Miscellany
-- ===================================================================

return {
  -- =================================================================
  --  vim-matchup
  -- -----------------------------------------------------------------
  --  https://github.com/andymass/vim-matchup
  --  Extend delimiter matching support
  -- -----------------------------------------------------------------
  "andymass/vim-matchup",

  -- =================================================================
  --  abolish.vim
  -- -----------------------------------------------------------------
  --  https://github.com/tpope/vim-abolish
  --  Work with several variants of a word at once
  -- -----------------------------------------------------------------
  "tpope/vim-abolish",

  -- =================================================================
  --  eunuch.vim
  -- -----------------------------------------------------------------
  --  https://github.com/tpope/vim-eunuch
  --  Commands for common Unix programs
  -- -----------------------------------------------------------------
  { "tpope/vim-eunuch", event = "CmdlineEnter" },

  -- =================================================================
  --  projectionist.vim
  -- -----------------------------------------------------------------
  --  https://github.com/tpope/vim-projectionist
  --  Project management
  -- -----------------------------------------------------------------
  "tpope/vim-projectionist",

  -- =================================================================
  --  repeat.vim
  -- -----------------------------------------------------------------
  --  https://github.com/tpope/vim-repeat
  --  Extend repeat action
  -- -----------------------------------------------------------------
  "tpope/vim-repeat",

  -- =================================================================
  --  rsi.vim
  -- -----------------------------------------------------------------
  --  https://github.com/tpope/vim-rsi
  --  Readline bindings for insert and command-line mode
  -- -----------------------------------------------------------------
  { "tpope/vim-rsi" },

  -- =================================================================
  --  unimpaired.vim
  -- -----------------------------------------------------------------
  --  https://github.com/tpope/vim-unimpaired
  --  Convenient paired bracket mappings
  -- -----------------------------------------------------------------
  "tpope/vim-unimpaired",

  -- =================================================================
  --  vim-slime
  -- -----------------------------------------------------------------
  --  https://github.com/jpalardy/vim-slime
  --  Send text from buffer to various targets
  -- -----------------------------------------------------------------
  {
    "jpalardy/vim-slime",
    config = function()
      vim.g.slime_target = "zellij"
    end,
  },

  -- =================================================================
  --  colorizer.lua
  -- -----------------------------------------------------------------
  --  https://github.com/NvChad/nvim-colorizer.lua
  --  Highlight color codes in buffer
  -- -----------------------------------------------------------------
  {
    "NvChad/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    ft = { "html", "css", "javascript", "svg", "typescript" },
    opts = {
      user_default_options = {
        AARRGGBB = true,
        RGB = true,
        RRGGBB = true,
        RRGGBBAA = true,
        hsl_fn = true,
        names = false,
        rgb_fn = true,
      },
    },
  },
}
