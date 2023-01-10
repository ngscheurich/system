local M = { "nvim-lualine/lualine.nvim" }

function M.config()
  require("lualine").setup({
    options = {
      component_separators = "",
      section_separators = "",
    },

    sections = require("plugins.lualine.sections"),
  })
end

return M
