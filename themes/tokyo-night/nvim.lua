vim.opt.background = "dark"

require("tokyonight").setup({
  style = "night",
})

vim.cmd("colorscheme tokyonight")


_G.theme = {
  statusbar = {
    bg       = "#292e42",
    bg_light = "#414868",
    fg       = "#c0caf5",
    fg_dark  = "#a9b1d6",
    blue     = "#7aa2f7",
    green    = "#9ece6a",
    red      = "#f7768e",
    yellow   = "#e0af68",
  }
}
