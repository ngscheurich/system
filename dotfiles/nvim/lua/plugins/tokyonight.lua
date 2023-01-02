local M = { "folke/tokyonight.nvim" }

function M.config()
  require("tokyonight").setup({
    style = "night",
    sidebars = { "qf", "terminal", "help", "Outline" },
  })

  vim.cmd.colorscheme("tokyonight")
end

return M
