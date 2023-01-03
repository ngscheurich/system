local M = { "nvim-lualine/lualine.nvim" }

function M.config()
  local nonicons = require("nvim-nonicons")

  require("lualine").setup({
    options = {
      component_separators = "",
      section_separators = "",
    },

    sections = {
      lualine_b = {
        {
          "branch",
          icon = nonicons.get("git-branch"),
        },
      },
    },
  })
end

return M
