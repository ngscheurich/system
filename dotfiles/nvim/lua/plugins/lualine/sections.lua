local M = {}

local nonicons = require("nvim-nonicons")
local components = require("plugins.lualine.components")
local git = require("util").git

local separators = { a = "▒", b = "▓", y = "▓", z = "░" }

local function get_option(opt)
  return vim.api.nvim_buf_get_option(0, opt)
end

M.lualine_a = {
  {
    "mode",
    color = { gui = "bold" },
    separator = { left = separators.a },
  },
}

M.lualine_b = {
  {
    "branch",
    fmt = function(branch)
      return git.format_branch(branch)
    end,
    icon = nonicons.get("git-branch"),
    separator = { right = separators.b },
  },
}

M.lualine_c = {
  {
    "filename",
    symbols = { modified = "", readonly = "" },
  },
  "diff",
  {
    "encoding",
    cond = function()
      return get_option("fileencoding") ~= "utf-8"
    end,
  },
  {
    "fileformat",
    cond = function()
      return get_option("fileformat") ~= "unix"
    end,
  },
}

M.lualine_x = {
  {
    "diagnostics",
    sources = { "nvim_diagnostic" },
  },
  components.navic,
  "filetype",
}

M.lualine_y = {
  {
    "progress",
    separator = { left = separators.y },
  },
}

M.lualine_z = {
  {
    "location",
    separator = { right = separators.z },
  },
}

return M
