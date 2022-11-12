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
ensure("https://github.com/wbthomason/packer.nvim")

require("hotpot")
require("conf")

local dap = require("dap")

dap.adapters.mix_task = {
  type = "executable",
  command = "/Users/nscheurich/.local/share/nvim/mason/bin/elixir-ls-debugger",
  args = {},
}

dap.configurations.elixir = {
  {
    type = "mix_task",
    name = "mix test",
    task = 'test',
    taskArgs = { "--trace" },
    request = "launch",
    startApps = true, -- for Phoenix projects
    projectDir = "${workspaceFolder}",
    requireFiles = {
      "test/**/test_helper.exs",
      "test/**/*_test.exs"
    }
  },
}
