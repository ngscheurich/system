local M = { "nvim-lualine/lualine.nvim" }

function M.config()
  local nonicons = require("nvim-nonicons")

  require("lualine").setup({
    options = {
      component_separators = "",
      section_separators = "",
    },

    sections = {
      lualine_a = {
        {
          "mode",
          separator = {},
        },
      },
      lualine_b = {
        {
          "branch",
          icon = nonicons.get("git-branch"),
          section_separators = { left = "" },
        },
      },
      lualine_c = {
        {
          "filename",
          symbols = {
            modified = "",
            readonly = "",
            unnamed = "",
            newfile = "",
          },
        },
        "diff",
        {
          "diagnostics",
          sources = { "nvim_diagnostic" },
        },
      },
      lualine_x = {
        "encoding",
        {
          "fileformat",
          symbols = {
            unix = "unix",
          },
        },
        "filetype",
      },
      lualine_y = {
        {
          "progress",
          section_separators = { right = "" },
        },
      },
    },
  })
end

return M
