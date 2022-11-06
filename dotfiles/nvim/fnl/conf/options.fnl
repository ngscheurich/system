(local opt vim.opt)

(tset opt :completeopt ["menuone" "noselect"])
(tset opt :cursorline true)
(tset opt :cursorcolumn false)
(tset opt :expandtab true)
(tset opt :fillchars "vert:│")
(tset opt :foldlevel 99)
(tset opt :foldmethod :syntax)
(tset opt :laststatus 3)
(tset opt :grepprg "rg --vimgrep")
(tset opt :hidden true)
(tset opt :ignorecase true)
(tset opt :inccommand "split")
(tset opt :mouse "a")
(tset opt :showmode false)
(tset opt :swapfile false)
(tset opt :number true)
(tset opt :relativenumber true)
(tset opt :scrolloff 10)
(tset opt :shiftwidth 2)
(tset opt :signcolumn "yes")
(tset opt :smartcase true)
(tset opt :smartindent true)
(tset opt :softtabstop 2)
(tset opt :splitbelow true)
(tset opt :splitright true)
(tset opt :termguicolors true)
(tset opt :tabstop 2)
(tset opt :undofile true)
(tset opt :updatetime 1000)

(table.insert opt.shortmess "c")

(tset opt :listchars {:tab ">-"
                      :eol "↵"
                      :nbsp "␣"
                      :trail "‧"
                      :extends "⟩"
                      :precedes "⟨"})
