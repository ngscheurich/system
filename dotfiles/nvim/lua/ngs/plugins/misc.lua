return {
  -- Expanded matching delimiters
  "andymass/vim-matchup",

  -- Directory buffers
  "justinmk/vim-dirvish",

  -- Improved substitution
  "tpope/vim-abolish",

  -- Commands for common Unix programs
  "tpope/vim-eunuch",

  -- Project management
  "tpope/vim-projectionist",

  -- Expanded repeat
  "tpope/vim-repeat",

  -- Readline bindings
  "tpope/vim-rsi",

  -- Paired bracket mappings
  "tpope/vim-unimpaired",

  -- Send text to terminal multiplexer
  {
    "jpalardy/vim-slime",
    config = function()
      vim.g.slime_target = "neovim"
    end,
  },

  -- Highlight color codes
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
