-- [nfnl] Compiled from fnl/statusline.fnl by https://github.com/Olical/nfnl, do not edit.
local _local_1_ = require("nfnl.core")
local get_in = _local_1_["get-in"]
local merge = _local_1_["merge"]
local reduce = _local_1_["reduce"]
local some = _local_1_["some"]
local _local_2_ = require("catppuccin.palettes")
local get_palette = _local_2_["get_palette"]
local heirline = require("heirline")
local conds = require("heirline.conditions")
local utils = require("heirline.utils")
local function catppuccin_color(name, dim)
  local color = get_palette("mocha")[name]
  return Snacks.util.blend("#000000", color, (dim or 0))
end
local function get_hl_attr(hl_group, attr)
  local hl_id = vim.fn.hlID(hl_group)
  return vim.fn.synIDattr(hl_id, attr)
end
local function hl(fg, bg)
  return {fg = catppuccin_color((fg or "text")), bg = catppuccin_color((bg or "mantle"))}
end
local function get_diagnostic_sign(severity)
  return get_in(vim.diagnostic.config(), {"signs", "text", severity})
end
local function get_diagnostic_count(severity)
  return #vim.diagnostic.get(0, {severity = severity})
end
local function get_mode_opts(mode)
  local list = {{modes = {"n", "niI", "niR", "niV", "nt", "nT"}, name = "NORMAL", color = "blue", icon = "\238\174\148 "}, {modes = {"no", "nov", "noV", "no\22"}, name = "NORMAL", color = "pink", icon = "\243\177\166\159 "}, {modes = {"v", "vs", "V", "Vs"}, name = "VISUAL", color = "pink", icon = "\243\176\146\133 "}, {modes = {"\22", "\22s"}, name = "VISUAL", color = "pink", icon = "\243\176\169\172 "}, {modes = {"s", "S", "\19"}, name = "SELECT", color = "rosewater", icon = "\243\176\171\153 "}, {modes = {"i", "ic", "ix"}, name = "INSERT", color = "green", icon = "\238\169\160 "}, {modes = {"R", "Rc", "Rx", "Rv", "Rvc", "Rvx"}, name = "REPLACE", color = "red", icon = "\238\172\189 "}, {modes = {"c"}, name = "COMMAND", color = "flamingo", icon = "\239\146\181 "}, {modes = {"cv"}, name = "EX", color = "flamingo", icon = "\238\173\173 "}, {modes = {"r", "rm", "r?", "!"}, name = "...", color = "lavender", icon = "\243\176\134\133 "}, {modes = {"t"}, name = "TERMINAL", color = "sapphire", icon = "\239\146\137 "}}
  local mode_opts = nil
  for _, t in ipairs(list) do
    if (mode_opts ~= nil) then break end
    local function _3_(_241)
      return (_241 == mode)
    end
    if (some(_3_, t.modes) or false) then
      mode_opts = t
    else
    end
  end
  return mode_opts
end
local mode_init
local function _5_(_241)
  _241["mode"] = vim.fn.mode(1)
  return nil
end
mode_init = _5_
local mode_update
local function _6_()
  return vim.cmd("redrawstatus")
end
mode_update = {"ModeChanged", pattern = "*:*", callback = vim.schedule_wrap(_6_)}
local mode_bar
local function _7_(_241)
  return string.format(" %s ", get_mode_opts(_241.mode).icon)
end
local function _8_(self)
  local _9_
  do
    local _let_10_ = get_mode_opts(self.mode)
    local color = _let_10_["color"]
    _9_ = catppuccin_color(color, 0.2)
  end
  return {fg = catppuccin_color("base"), bg = _9_}
end
local function _11_(_241)
  return string.format(" %s ", get_mode_opts(_241.mode).name)
end
local function _12_(self)
  local _13_
  do
    local _let_14_ = get_mode_opts(self.mode)
    local color = _let_14_["color"]
    _13_ = catppuccin_color(color)
  end
  return {fg = catppuccin_color("base"), bg = _13_, bold = true}
end
mode_bar = {{provider = _7_, hl = _8_}, {provider = _11_, hl = _12_}, init = mode_init, update = mode_update}
local mode_tag
local function _15_()
  return " "
end
local function _16_(self)
  local _17_
  do
    local _let_18_ = get_mode_opts(self.mode)
    local color = _let_18_["color"]
    _17_ = catppuccin_color(color, 0.2)
  end
  return {bg = _17_}
end
mode_tag = {init = mode_init, update = mode_update, provider = _15_, hl = _16_}
local function split_path(name)
  local last_slash = string.match(name, ".*/()")
  if last_slash then
    return string.sub(name, 1, (last_slash - 1)), string.sub(name, last_slash)
  else
    return "", name
  end
end
local function file_path_provider(_20_)
  local filename = _20_["filename"]
  local name = vim.fn.fnamemodify(filename, ":.")
  local path = split_path(name)
  if not conds.width_percent_below(#path, 0.2) then
    return vim.fn.pathshorten(path)
  else
    return path
  end
end
local function file_name_provider(_22_)
  local filename = _22_["filename"]
  local _23_ = vim.fn.fnamemodify(filename, ":t")
  if (_23_ == "") then
    return "[No Name]"
  elseif (nil ~= _23_) then
    local name = _23_
    return name
  else
    return nil
  end
end
local file
local function _25_()
  if vim.bo.modified then
    return hl("yellow")
  else
    return nil
  end
end
local function _27_()
  return vim.bo.modified
end
local function _28_()
  return (not vim.bo.modifiable or vim.bo.readonly)
end
local function _29_(_241)
  _241["filename"] = vim.api.nvim_buf_get_name(0)
  return nil
end
file = {{provider = file_path_provider, hl = hl("subtext0")}, {provider = file_name_provider, hl = _25_}, {condition = _27_, provider = "\239\145\132", hl = hl("peach")}, {condition = _28_, provider = " \239\128\163", hl = hl("flamingo")}, init = _29_}
local function git_diff_provider(self, type, sym)
  local count = (self.status[type] or 0)
  if (count > 0) then
    return (sym .. count)
  else
    return nil
  end
end
local git
local function _31_(_241)
  return string.format("\239\144\152 %s ", _241.status.head)
end
local function _32_(_241)
  return git_diff_provider(_241, "added", "+")
end
local function _33_(_241)
  return git_diff_provider(_241, "removed", "-")
end
local function _34_(_241)
  return git_diff_provider(_241, "changed", "~")
end
local function _35_(self)
  self["status"] = vim.b.gitsigns_status_dict
  return nil
end
git = {{provider = _31_}, {provider = _32_, hl = hl("green")}, {provider = _33_, hl = hl("red")}, {provider = _34_, hl = hl("yellow")}, condition = conds.is_git_repo, init = _35_, hl = hl("pink")}
local function lsp_provider(self)
  local names = {}
  for _, _36_ in ipairs(vim.lsp.get_clients({bufnr = 0})) do
    local name = _36_["name"]
    local function _37_(_241)
      return (_241 == name)
    end
    if not some(_37_, self.hidden) then
      table.insert(names, name)
    else
    end
  end
  if (#names > 0) then
    return ("\239\144\163  " .. table.concat(names, " | "))
  else
    return nil
  end
end
local lsp = {condition = conds.lsp_attached, update = {"LspAttach", "LspDetach"}, static = {hidden = {"GitHub Copilot"}}, provider = lsp_provider, hl = hl("blue")}
local diagnostics
local function _40_(_241)
  return ((_241.errors > 0) and (" " .. _241.icons.error .. " " .. _241.errors))
end
local function _41_(_241)
  return ((_241.warns > 0) and (" " .. _241.icons.warn .. " " .. _241.warns))
end
local function _42_(_241)
  return ((_241.infos > 0) and (" " .. _241.icons.info .. " " .. _241.infos))
end
local function _43_(_241)
  return ((_241.hints > 0) and (" " .. _241.icons.hint .. " " .. _241.hints))
end
local function _44_(self)
  self["errors"] = get_diagnostic_count("ERROR")
  self["warns"] = get_diagnostic_count("WARN")
  self["infos"] = get_diagnostic_count("INFO")
  self["hints"] = get_diagnostic_count("HINT")
  return nil
end
diagnostics = {{provider = _40_, hl = hl("red")}, {provider = _41_, hl = hl("yellow")}, {provider = _42_, hl = hl("teal")}, {provider = _43_, hl = hl("sapphire")}, condition = conds.has_diagnostics, static = {icons = {error = get_diagnostic_sign(vim.diagnostic.severity.ERROR), warn = get_diagnostic_sign(vim.diagnostic.severity.WARN), info = get_diagnostic_sign(vim.diagnostic.severity.INFO), hint = get_diagnostic_sign(vim.diagnostic.severity.HINT)}}, init = _44_, update = {"DiagnosticChanged", "BufEnter"}}
local filetype
local function _45_()
  local icon = MiniIcons.get("filetype", vim.bo.filetype)
  return icon
end
local function _46_()
  local _, hl_group = MiniIcons.get("filetype", vim.bo.filetype)
  return {fg = get_hl_attr(hl_group, "fg"), bg = catppuccin_color("mantle")}
end
local function _47_()
  return (" " .. vim.bo.filetype)
end
filetype = {{provider = _45_, hl = _46_}, {provider = _47_, hl = hl("subtext1")}}
local ruler = {provider = " %7(%l/%3L%):%2c %P ", hl = {bg = catppuccin_color("surface1")}}
local function gap(n)
  local function _48_()
    local g = ""
    for _ = 1, (n or 1) do
      g = (g .. " ")
    end
    return g
  end
  return {provider = _48_}
end
local function _49_()
  return heirline.setup({statusline = {mode_bar, gap(2), file, gap(2), git, {provider = "%="}, diagnostics, gap(2), lsp, gap(2), filetype, gap(2), ruler, mode_tag}})
end
return {setup = _49_}
