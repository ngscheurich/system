local M = { "nvim-lualine/lualine.nvim" }

function M.config()
  local icons = require("nvim-nonicons")

  require("lualine").setup({
    options = {
      component_separators = "",
      section_separators = "",
    },

    sections = {
      lualine_b = {
        {
          "branch",
          icon = icons.get("git-branch"),
        },
      },
    },
  })
end

return M
