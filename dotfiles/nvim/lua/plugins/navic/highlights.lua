local M = {}

local function get_colors()
  local colors = {}

  if _G.colorscheme == "tokyonight-night" then
    local palette = require("tokyonight.colors")
    colors = {
      fg = palette.default.fg,
      bg = palette.night.bg_dark,
      icon = palette.default.blue,
      separator = palette.default.comment,
    }
  elseif _G.colorscheme == "nordfox" then
    local palette = require('nightfox.palette').load("nightfox")
    colors = {
      fg = palette.fg1,
      bg = "#20242d",
      icon = palette.blue.dim,
      separator = palette.separator,
    }
  end

  return colors
end

local function set_highlight(group, fg, bg)
  vim.api.nvim_set_hl(0, group, { fg = fg, bg = bg })
end

function M.setup()
  local colors = get_colors()

  local icons = {
    "Array",
    "Boolean",
    "Class",
    "Constant",
    "Constructor",
    "Enum",
    "EnumMember",
    "Event",
    "Field",
    "File",
    "Function",
    "Interface",
    "Key",
    "Method",
    "Module",
    "Namespace",
    "Null",
    "Number",
    "Object",
    "Operator",
    "Package",
    "Property",
    "String",
    "Struct",
    "TypeParameter",
    "Variable",
  }

  for _, icon in ipairs(icons) do
    set_highlight("NavicIcons" .. icon, colors.icon, colors.bg)
  end

  set_highlight("NavicText", colors.fg, colors.bg)
  set_highlight("NavicSeparator", colors.separator, colors.bg)
end

return M
