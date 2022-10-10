vim.opt.background = "dark"
vim.cmd("colorscheme alchemist")

vim.cmd [[
hi SignColumn   guibg=#2b2b2b
hi LineNr       guibg=#2b2b2b
hi CursorLineNr guibg=#2b2b2b

hi WinSeparator guifg=#383838 guibg=None

hi GitGutterAdd          guifg=#5f875f guibg=#2b2b2b
hi GitGutterChange       guifg=#87875f guibg=#2b2b2b
hi GitGutterDelete       guifg=#af5f5f guibg=#2b2b2b
hi GitGutterChangeDelete guifg=#5f5f87 guibg=#2b2b2b

hi DiagnosticError       guifg=#af5f5f guibg=#262626
hi DiagnosticSignError   guifg=#af5f5f guibg=#2b2b2b
hi DiagnosticHint        guifg=#5f8787 guibg=#262626
hi DiagnosticSignHint    guifg=#5f8787 guibg=#2b2b2b
hi DiagnosticSignInfo    guifg=#5f5f87 guibg=#262626
hi DiagnosticSignOther   guifg=#5f5f87 guibg=#2b2b2b
hi DiagnosticWarn        guifg=#87875f guibg=#262626
hi DiagnosticSignWarn    guifg=#87875f guibg=#2b2b2b

hi NvimTreeNormal     guibg=#1e1e1e
hi NvimTreeNormalNC   guibg=#1e1e1e
hi NvimTreeRootFolder guibg=#1e1e1e
]]

_G.theme = {
  statusbar = {
    bg       = "#3a3a3a",
    bg_light = "#585858",
    fg       = "#ffffff",
    fg_dark  = "#bcbcbc",
    blue     = "#5f87af",
    green    = "#5f875f",
    red      = "#af5f5f",
    yellow   = "#87875f",
  }
}
