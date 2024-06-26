-- ===================================================================
--  Options
-- ===================================================================

local indent = 2

local listchars = {
  tab = ">-",
  eol = "↵",
  nbsp = "␣",
  trail = "‧",
  extends = "⟩",
  precedes = "⟨",
}

-- Interface
vim.opt.conceallevel = 2
vim.opt.cursorline = true
vim.opt.fillchars = { vert = "│" }
vim.opt.laststatus = 3
vim.opt.listchars = listchars
vim.opt.number = true
vim.opt.scrolloff = 12
vim.opt.shortmess:append("c")
vim.opt.showmode = false
vim.opt.sidescrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true

-- Indentation
vim.opt.breakindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = indent
vim.opt.smartindent = true
vim.opt.softtabstop = indent
vim.opt.tabstop = indent

-- Search
vim.opt.grepprg = "rg --vimgrep"
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.smartcase = true

-- Completion
vim.opt.completeopt = { "menu", "menuone", "noinsert" }
vim.opt.pumheight = 10

-- Folding
vim.opt.foldcolumn = "0"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevel = 99
vim.opt.foldmethod = "expr"
vim.opt.foldtext = ""

-- Behavior
vim.opt.hidden = true
vim.opt.timeoutlen = 250
vim.opt.undofile = true
vim.opt.updatetime = 250
vim.opt.clipboard = "unnamedplus"
