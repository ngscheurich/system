return {
  "SmiteshP/nvim-navic",

  cond = false,

  config = function()
    vim.g.navic_silence = true

    require("nvim-navic").setup({
      icons = {
        Number = "# ",
        String = " ",
      },
      separator = "  ",
      depth_limit = 3,
      depth_limit_indicator = "..",
      highlight = true,
    })

    require("plugins.navic.highlights").setup(_G.colorscheme)
  end,
}
