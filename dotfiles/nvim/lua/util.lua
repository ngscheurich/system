M = {}

local fn = vim.fn

---Returns a lazy.nvim plugin spec for nvim-treesitter with the given parser
---added to opts.ensure_installed.
---@param parser string
---@return table
M.treesitter_ensure = function(parser)
  return {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      return {
        ensure_installed = vim.list_extend(opts.ensure_installed or {}, { parser }),
      }
    end,
  }
end

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
          [server] = config or {},
        }),
      }
    end,
  }
end

---Returns a lazy.nvim plugin spec for LuaSnip with snippets configured for lang.
---@param lang string
---@param snippets table
---@return table
M.luasnip_add = function(lang, snippets)
  return {
    "L3MON4D3/LuaSnip",
    -- opts = function(_, opts)
    --   return {
    --     snippets = vim.tbl_extend("error", opts.snippets or {}, {
    --       [lang] = snippets,
    --     }),
    --   }
    -- end,
    opts = function(_, opts)
      return M.set_opt(opts, "snippets", lang, snippets)
    end,
  }
end

---Returns a lazy.nvim plugin spec for Conform with formatters configured for ft.
---@param ft string
---@param formatters table
---@return table
M.formatter_setup = function(ft, formatters)
  return {
    "stevearc/conform.nvim",
    -- opts = function(_, opts)
    --   return vim.tbl_extend("force", opts, {
    --     formatters_by_ft = vim.tbl_extend("error", opts.formatters_by_ft or {}, {
    --       [filetype] = list,
    --     }),
    --   })
    -- end,
    opts = function(_, opts)
      return M.set_opt(opts, "formatters_by_ft", ft, formatters)
    end,
  }
end

---Returns a lazy.nvim plugin spec for nvim-lint with linters configured for ft.
---@param ft string
---@param linters table
---@return table
M.linter_setup = function(ft, linters)
  return {
    "mfussenegger/nvim-lint",
    opts = function(_, opts)
      return M.set_opt(opts, "linters_by_ft", ft, linters)
    end,
  }
end

---Returns opts with key set to val under name.
---@param opts table
---@param name string
---@param key string
---@param val any
---@return table
M.set_opt = function(opts, name, key, val)
  return vim.tbl_extend("force", opts, {
    [name] = vim.tbl_extend("error", opts[name] or {}, {
      [key] = val,
    }),
  })
end

---Registers an autocmd to lint the current ft buffer with nvim-lint.
---@param ft string
---@return nil
M.create_lint_autocmd = function(ft)
  local ok, lint = pcall(require, "lint")

  if ok then
    vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost" }, {
      desc = "Lints " .. ft .. "buffer",
      group = vim.api.nvim_create_augroup("lint-buffer", { clear = false }),
      buffer = 0,
      callback = function()
        lint.try_lint()
      end,
    })
  end
end

---Calls the callback function for each module under the given namespace.
---@param namespace string
---@param cb function
---@return nil
M.foreach_module = function(namespace, cb)
  local conf = fn.stdpath("config")
  local dir = string.format("%s/lua/%s/", conf, string.gsub(namespace, "%.", "/"))
  local paths = vim.split(fn.glob(dir .. "*lua"), "\n")

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

---Launches Telescope with the given picker, optionally, theme and options.
---@param picker string
---@param theme? string
---@param opts? table
---@return nil
function M.pick(picker, theme, opts)
  local borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }

  if theme == "get_dropdown" then
    borderchars = {
      prompt = borderchars,
      results = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
      preview = borderchars,
    }
  elseif theme == "get_ivy" then
    borderchars = {
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
      preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
    }
  end

  opts = vim.tbl_deep_extend("error", opts or {}, { borderchars = borderchars })

  return function()
    if theme then
      require("telescope.builtin")[picker](require("telescope.themes")[theme](opts))
    else
      require("telescope.builtin")[picker](opts)
    end
  end
end

---Returns the value of a highlight group attribute
---@param group string
---@param attr string
---@return string
function M.get_highlight_group_attr(group, attr)
  return fn.synIDattr(fn.synIDtrans(fn.hlID(group)), attr)
end

---Applies the globally-configured colorscheme.
---@return nil
function M.apply_colorscheme()
  local colorscheme = _G.theme.colorscheme.name
  local callback = _G.theme.callback
  vim.cmd.colorscheme(colorscheme)
  pcall(callback, {})
end

return M
