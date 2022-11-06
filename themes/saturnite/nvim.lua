vim.opt.background = "dark"
vim.cmd("colorscheme saturnite")

vim.cmd [[
hi CursorLine guibg=#2d2d2d

hi SignColumn   guibg=#232323
hi LineNr       guibg=#232323
hi CursorLineNr guibg=#232323

hi WinSeparator guifg=#383838 guibg=None

hi GitGutterAdd          guifg=#5f875f guibg=#232323
hi GitGutterChange       guifg=#af8700 guibg=#232323
hi GitGutterDelete       guifg=#af5f5f guibg=#232323
hi GitGutterChangeDelete guifg=#5f87af guibg=#232323

hi DiagnosticError       guifg=#af5f5f guibg=#1c1c1c
hi DiagnosticSignError   guifg=#af5f5f guibg=#232323
hi DiagnosticHint        guifg=#5f875f guibg=#1c1c1c
hi DiagnosticSignHint    guifg=#5f875f guibg=#232323
hi DiagnosticSignInfo    guifg=#5f87af guibg=#1c1c1c
hi DiagnosticSignOther   guifg=#5f87af guibg=#232323
hi DiagnosticWarn        guifg=#af8700 guibg=#1c1c1c
hi DiagnosticSignWarn    guifg=#af8700 guibg=#232323

hi NvimTreeNormal     guibg=#1e1e1e
hi NvimTreeNormalNC   guibg=#1e1e1e
hi NvimTreeRootFolder guibg=#1e1e1e

hi link TelescopeSelection CursorLine
]]

_G.theme = {
  statusbar = {
    bg       = "#2d2d2d",
    bg_light = "#3a3a3a",
    fg       = "#bcbcbc",
    fg_dark  = "#9c9c9c",
    blue     = "#5f87af",
    green    = "#5f875f",
    red      = "#af5f5f",
    yellow   = "#af8700",
  }
}
