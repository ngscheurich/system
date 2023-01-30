local M = { "EdenEast/nightfox.nvim" }

M.cond = _G.colorscheme == "nordfox"

function M.config()
  if _G.colorscheme == "nordfox" then
    require("nightfox").setup({
      palettes = {
        nordfox = {
          bg0 = "#20242d",
          bg1 = "#252a33",
          bg2 = "#2e3440",
          bg3 = "#3b4252",
          bg4 = "#434853",
        },
      },
    })

    vim.cmd.colorscheme("nordfox")
  end
end

return M
