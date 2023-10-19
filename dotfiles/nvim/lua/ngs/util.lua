M = {}

---Returns a lazy.nvim plugin spec for nvim-lspconfig with the given config
---set as the value of opts.servers[server].
---@param server string
---@param config table | nil
---@return table
M.lspconfig_setup = function(server, config)
  return {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      return {
        servers = vim.tbl_extend("error", opts.servers or {}, {
          [server] = config or {}
        })
      }
    end
  }
end

---Returns a lazy.nvim plugin spec for nvim-treesitter with the given parser
---added to opts.ensure_installed.
---@param parser string
---@return table
M.treesitter_ensure = function(parser)
  return {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      return {
        ensure_installed = vim.list_extend(opts.ensure_installed or {}, { parser })
      }
    end,
  }
end

---Calls the callback function for each module under the given namespace.
---@param namespace string
---@param cb function
---@return nil
M.foreach_module = function(namespace, cb)
  local conf = vim.fn.stdpath("config")
  local dir = string.format("%s/lua/%s/", conf, string.gsub(namespace, "%.", "/"))
  local paths = vim.split(vim.fn.glob(dir .. "*lua"), "\n")

  for _, p in ipairs(paths) do
    local name = string.sub(p, string.len(dir) + 1, string.len(p) - 4)
    if name ~= "init" then
      cb(string.format("%s.%s", namespace, name))
    end
  end
end

---Toggles a Neovim option.
---@param name string
---@return nil
M.toggle_opt = function(name)
  local on, off

  if name == "signcolumn" then
    on, off = "yes", "no"
  else
    on, off = true, false
  end

  if vim.o[name] == on then
    vim.o[name] = off
  else
    vim.o[name] = on
  end
end

---Removes the Shortcut prefix from a Git branch name.
---@param branch string
---@return string
function M.format_branch(branch)
  local match = string.match(branch, "^.*/sc%-%d+")

  if match then
    branch = match
  end

  return branch
end

return M
