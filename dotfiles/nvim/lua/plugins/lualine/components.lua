local M = {}

function M.navic()
  local navic = require("nvim-navic")

  if navic.is_available() then
    return require("nvim-navic").get_location()
  end

  return ""
end

return M
