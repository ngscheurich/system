local M = { "folke/tokyonight.nvim" }

M.cond = _G.colorscheme == "tokyonight"

function M.config()
  require("tokyonight").setup({
    style = "night",
    sidebars = { "qf", "terminal", "help", "Outline" },
  })

  vim.cmd.colorscheme("tokyonight")
end

return M
