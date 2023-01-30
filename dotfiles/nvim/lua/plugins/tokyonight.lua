local M = { "folke/tokyonight.nvim" }

M.cond = _G.colorscheme == "tokyonight-night"

function M.config()
  if _G.colorscheme == "tokyonight-night" then
    require("tokyonight").setup({
      style = "night",
      sidebars = { "qf", "terminal", "help", "Outline" },
    })

    vim.cmd.colorscheme("tokyonight")
  end
end

return M
