vim.opt.background = "dark"
vim.cmd("colorscheme base16-tomorrow-night")

vim.cmd [[
hi SignColumn   guibg=#202225
hi LineNr       guibg=#202225
hi CursorLineNr guibg=#202225

hi WinSeparator guifg=#363a41 guibg=None

hi MatchParen   guibg=None gui=underline

hi GitGutterAdd          guibg=#202225
hi GitGutterChange       guibg=#202225
hi GitGutterDelete       guibg=#202225
hi GitGutterChangeDelete guibg=#202225

hi DiagnosticError       guifg=#cc6666 guibg=#291414
hi DiagnosticSignError   guifg=#cc6666 guibg=#202225
hi DiagnosticHint        guifg=#8abeb7 guibg=#1a2026
hi DiagnosticSignHint    guifg=#8abeb7 guibg=#202225
hi DiagnosticSignInfo    guifg=#b4b7b4 guibg=#242524
hi DiagnosticSignOther   guifg=#b4b7b4 guibg=#202225
hi DiagnosticWarn        guifg=#f0c674 guibg=#302717
hi DiagnosticSignWarn    guifg=#f0c674 guibg=#202225

hi NvimTreeNormal     guibg=#141517
hi NvimTreeNormalNC   guibg=#141517
hi NvimTreeRootFolder guibg=#141517
]]


_G.theme = {
  statusbar = {
    bg       = "#282a2e",
    bg_light = "#363a41",
    fg       = "#c4c8c5",
    fg_dark  = "#b0b4b1",
    blue     = "#80a1bd",
    green    = "#8abdb6",
    red      = "#cc6666",
    yellow   = "#f0c574",
  }
}
