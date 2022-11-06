local cmd, opt = vim.cmd, vim.opt
local colorscheme = "base16-tomorrow-night"

local apply_colors = function()
  local light_bg = {
    "CursorLineNr",
    "GitGutterAdd",
    "GitGutterChange",
    "GitGutterChangeDelete",
    "GitGutterDelete",
    "LineNr",
    "SignColumn",
  }
  for _, group in ipairs(light_bg) do
    cmd(string.format("hi %s guibg=#212326", group))
  end

  local dark_bg = {
    "NvimTreeNormal",
    "NvimTreeNormalNC",
    "NvimTreeRootFolder"
  }
  for _, group in ipairs(dark_bg) do
    cmd(string.format("hi %s guibg=#141517", group))
  end

  cmd("hi GitGutterAdd          guifg=#b5bd68")
  cmd("hi GitGutterChange       guifg=#f0c574")
  cmd("hi GitGutterDelete       guifg=#cc6666")
  cmd("hi GitGutterChangeDelete guifg=#80a1bd")
end

vim.api.nvim_create_autocmd({ "ColorScheme" }, {
  pattern = { colorscheme },
  callback = apply_colors,
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "help" },
  callback = function()
    vim.opt_local.signcolumn = "no"
  end,
})

opt.background = "dark"
cmd("colorscheme " .. colorscheme)

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
