vim.opt.background = "dark"
local onedark = require("onedark")

onedark.setup({
  style = "dark",

  colors = {
    black  = "#0e1013",
    bg_d   = "#181b20",
    bg0    = "#1f2329",
    bg1    = "#282c34",
    bg2    = "#30363f",
    bg3    = "#323641",
    fg     = "#bdc2cd",
    purple = "#d295e4",
    blue   = "#56b6c2"
  },

  highlights = {
    CursorLineNr = { bg = "#21252b" },
    LineNr       = { bg = "#21252b" },
    SignColumn   = { bg = "#21252b" },
  }
})

onedark.load()

_G.theme = {
  statusbar = {
    bg       = "#282c34",
    bg_light = "#393e48",
    fg       = "#cccccc",
    fg_dark  = "#abb2bf",
    blue     = "#56b6c2",
    green    = "#98c379",
    red      = "#e06c75",
    yellow   = "#e5c07b",
  }
}
