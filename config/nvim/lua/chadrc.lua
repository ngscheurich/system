local M = {}

M.base46 = {
  -- theme = "poimandres",
  -- theme = "nightlamp",
  theme = "pasteldark",
  theme_toggle = { "pasteldark", "flex-light" },
}

M.ui = {
  cmp = {
    icons_left = true,
    style = "atom",
    format_colors = {
      tailwind = true,
    },
  },

  telescope = {
    style = "bordered",
  },

  statusline = {
    theme = "default",
    separator_style = "block",
  },
}

M.nvdash = {}
M.colorify = {}

return M
