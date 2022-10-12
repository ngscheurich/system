local function ensure(url)
  local name = url:gsub(".*/", "")
  local path = vim.fn.stdpath("data") .. "/site/pack/packer/start/" .. name

  if vim.fn.isdirectory(path) == 0 then
    print(name .. ": Installing to " .. path .. "...")
    vim.fn.system({ "git", "clone", "--depth", "1", url, path })
    vim.cmd("redraw")
    vim.cmd("packadd " .. name)
    print(name .. ": Installation complete!")
  end
end

ensure("https://github.com/rktjmp/hotpot.nvim")
ensure("https://github.com/ngscheurich/themis.nvim")
ensure("https://github.com/wbthomason/packer.nvim")

require("hotpot")
require("conf")
