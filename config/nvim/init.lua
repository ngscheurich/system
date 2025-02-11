-- [nfnl] Compiled from init.fnl by https://github.com/Olical/nfnl, do not edit.
local conf_dir = "/private/etc/system/config/nvim"
local conf_src = (conf_dir .. "/README.md")
local conf_out = (conf_dir .. "/init.lua")
local user_group = vim.api.nvim_create_augroup("ngs", {})
local function tangle_reload()
  vim.system({"make"}, {cwd = conf_dir}):wait()
  vim.cmd(("source " .. conf_out))
  return vim.cmd("redraw | echo '\243\176\145\147 Tangled and reloaded config'")
end
vim.api.nvim_create_autocmd({"BufWritePost"}, {pattern = conf_src, group = user_group, callback = tangle_reload})
vim.cmd.set((("packpath^=" .. vim.fn.stdpath("data")) .. "/site"))
vim.cmd("packadd nfnl")
local _local_1_ = require("nfnl.module")
local autoload = _local_1_["autoload"]
local _local_2_ = autoload("nfnl.core")
local assoc = _local_2_["assoc"]
local get_in = _local_2_["get-in"]
local merge = _local_2_["merge"]
local user_opts = {}
user_opts = merge(user_opts, {conceallevel = 2, cursorline = true, fillchars = {vert = "\226\148\130"}, laststatus = 3, listchars = {tab = ">-", eol = "\226\134\181", nbsp = "\226\144\163", trail = "\226\128\167", extends = "\226\159\169", precedes = "\226\159\168"}, number = true, scrolloff = 13, sidescrolloff = 8, signcolumn = "yes", splitbelow = true, splitright = true, termguicolors = true, showmode = false})
local function _3_(...)
  local indent = 2
  return {breakindent = true, expandtab = true, shiftwidth = indent, smartindent = true, softtabstop = indent, tabstop = indent}
end
user_opts = merge(user_opts, _3_(...))
user_opts = merge(user_opts, {grepprg = "rg --vimgrep", ignorecase = true, inccommand = "split", smartcase = true})
user_opts = merge(user_opts, {completeopt = {"menu", "menuone", "noinsert"}, pumheight = 10})
user_opts = merge(user_opts, {hidden = true, timeoutlen = 250, undofile = true, updatetime = 250, clipboard = "unnamedplus"})
for k, v in pairs(user_opts) do
  vim.opt[k] = v
end
assoc(vim.g, "mapleader", " ", "maplocalleader", ",")
do
  local t = {["<Left>"] = "<C-w>h", ["<Down>"] = "<C-w>j", ["<Up>"] = "<C-w>k", ["<Right>"] = "<C-w>l"}
  for k, v in pairs(t) do
    vim.keymap.set("n", k, v)
  end
end
local function toggle_opt(name)
  local on, off = nil
  if (name == "signcolumn") then
    on, off = "yes", "no"
  else
    on, off = true, false
  end
  if (vim.o[name] == on) then
    vim.o[name] = off
    return nil
  else
    vim.o[name] = on
    return nil
  end
end
local function _6_()
  return toggle_opt("number")
end
vim.keymap.set("n", "<Leader>un", _6_, {desc = "Line numbers"})
local function _7_()
  return toggle_opt("list")
end
vim.keymap.set("n", "<Leader>uw", _7_, {desc = "Whitespace"})
local function _8_()
  return toggle_opt("cursorline")
end
vim.keymap.set("n", "<Leader>uc", _8_, {desc = "Cursorline"})
vim.keymap.set("n", "<Esc>", "<Cmd>nohlsearch<CR>", {desc = "Stop highlighting matches"})
vim.keymap.set("t", "<Esc><Esc>", "<C-\\\\><C-n>", {desc = "Exit Terminal mode"})
do
  local lazypath = (vim.fn.stdpath("data") .. "/lazy/lazy.nvim")
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local args = {"git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath}
    local out = vim.fn.system(args)
    if (vim.v.shell_error ~= 0) then
      vim.api.nvim_echo({{"Failed to clone lazy.nvim:\n", "ErrorMsg"}, {out, "WarningMsg"}, {"\nPress any key to exit..."}}, true, {})
      vim.fn.getchar()
      os.exit(1)
    else
    end
  else
  end
  vim.opt.rtp:prepend(lazypath)
end
local function spec(plugin, tbl)
  return assoc(tbl, 1, plugin)
end
do
  local lazy = require("lazy")
  local function _11_(_, opts)
    local _let_12_ = require("nvim-treesitter.configs")
    local setup = _let_12_["setup"]
    return setup(opts)
  end
  local function _13_()
    local lc = require("lspconfig")
    local servers = {bashls = {}, lexical = {}, gdscript = {}, gopls = {}, lua_ls = {}, nil_ls = {}, rust_analyzer = {}, ts_ls = {}}
    for s, c in pairs(servers) do
      get_in(lc, {s, "setup"})(c)
    end
    return nil
  end
  lazy.setup({dev = {path = "~/Projects"}, spec = {spec("Olical/nfnl", {ft = "fennel"}), spec("ngscheurich/srcedit", {dev = true, opts = {}}), spec("nvim-treesitter/nvim-treesitter", {opts = {highlight = {enable = true}, indent = {enable = true}, ensure_installed = {"bash", "css", "elixir", "erlang", "gdscript", "go", "graphql", "html", "http", "javascript", "json", "kdl", "lua", "nix", "rust", "scss", "sql", "svelte", "typescript", "xml", "yaml"}}, config = _11_}), spec("neovim/nvim-lspconfig", {dependencies = {"hrsh7th/cmp-nvim-lsp"}, config = _13_}), {"Olical/conjure"}}, checker = {enabled = true}})
end
vim.cmd("colorscheme habamax")
return vim.cmd("set conceallevel=0")
