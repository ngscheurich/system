local M = { "EdenEast/nightfox.nvim" }

M.cond = _G.colorscheme == "nightfox" or _G.colorscheme == "dayfox"

function M.config()
  require("nightfox").setup()

  vim.api.nvim_create_autocmd("Colorscheme", {
    pattern = "*fox",
    callback = function(tbl)
      local colorscheme = tbl.match

      local theme = require("nightfox.util.lualine")(colorscheme)
      require('lualine').setup({ options = { theme = theme } })
      require("plugins.navic.highlights").setup(colorscheme)

      local palette = require('nightfox.palette').load(colorscheme)
      if colorscheme == "nightfox" then
        vim.cmd("highlight CursorLine guibg=" .. palette.bg2)
      elseif colorscheme == "dayfox" then
        vim.cmd("highlight CursorLine guibg=" .. palette.bg0)
      end
    end,
    group = vim.api.nvim_create_augroup("NightfoxColors", {}),
  })

  vim.cmd.colorscheme(_G.colorscheme)
end

return M
