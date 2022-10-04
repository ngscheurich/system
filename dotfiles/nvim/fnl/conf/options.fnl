(import-macros {: set!} :themis.opt)


(set! completeopt [:menuone :noselect])
(set! cursorline true)
(set! cursorcolumn false)
(set! expandtab true)
(set! fillchars "vert:│")
(set! foldlevel 99)
(set! foldmethod :syntax)
(set! laststatus 3)
(set! grepprg "rg --vimgrep")
(set! hidden true)
(set! ignorecase true)
(set! inccommand :split)
(set! mouse :a)
(set! showmode false)
(set! swapfile false)
(set! number true)
(set! relativenumber true)
(set! scrolloff 10)
(set! shiftwidth 2)
(set! signcolumn :yes)
(set! smartcase true)
(set! smartindent true)
(set! softtabstop 2)
(set! splitbelow true)
(set! splitright true)
(set! termguicolors true)
(set! tabstop 2)
(set! undofile true)
(set! updatetime 1000)
(set! shortmess+ :c)

(set! listchars {:tab :>-
                 :eol :↵
                 :nbsp :␣
                 :trail :‧
                 :extends :⟩
                 :precedes :⟨})
