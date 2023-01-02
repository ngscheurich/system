------------------------------------------------------------
-- Options
------------------------------------------------------------

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
vim.opt.cursorline = true
vim.opt.fillchars = { vert = "│" }
vim.opt.laststatus = 3
vim.opt.listchars = listchars
vim.opt.number = true
vim.opt.scrolloff = 8
vim.opt.shortmess:append "c"
vim.opt.showmode = false
vim.opt.sidescrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true

-- Indentation
vim.opt.expandtab   = true
vim.opt.shiftwidth  = indent
vim.opt.smartindent = true
vim.opt.softtabstop = indent
vim.opt.tabstop     = indent

-- Search
vim.opt.grepprg = "rg --vimgrep"
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.smartcase = true

-- Completion
vim.opt.completeopt = { "menu", "noselect" }
vim.opt.pumheight = 10

-- Behavior
vim.opt.clipboard = "unnamedplus"
vim.opt.hidden = true
vim.opt.undofile = true
vim.opt.updatetime = 1000
