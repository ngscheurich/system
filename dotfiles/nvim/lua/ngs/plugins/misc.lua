return {
  -- Expanded matching delimiters
  "andymass/vim-matchup",

  -- Improved substitution
  "tpope/vim-abolish",

  -- Commands for common Unix programs
  { "tpope/vim-eunuch", event = "CmdlineEnter" },

  -- Project management
  "tpope/vim-projectionist",

  -- Expanded repeat
  "tpope/vim-repeat",

  -- Readline bindings for
  { "tpope/vim-rsi", event = "CmdlineEnter" },

  -- Paired bracket mappings
  "tpope/vim-unimpaired",

  -- Send text to external target
  {
    "jpalardy/vim-slime",
    config = function()
      vim.g.slime_target = "zellij"
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
