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
  "tpope/vim-surround",
  -- Easily comment/uncomment
  { "echasnovski/mini.comment",   version = false, config = true },
  -- Automatically insert pairs
  { "echasnovski/mini.pairs",     version = false, config = true },
  -- Split and join arguments
  { "echasnovski/mini.splitjoin", version = false, config = true },
}
