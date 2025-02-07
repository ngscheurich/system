;; The Neovim configuration directory
(local conf-dir (vim.fn.stdpath :config))

;; The literate config file
(local conf-src (.. conf-dir :/README.md))

;; The tangled/compiled config file
(local conf-out (.. conf-dir :/init.lua))

;; An autocommand group to contain our autocommands
;; See `:h augroup`
(local user-group (vim.api.nvim_create_augroup :ngs {}))
(fn tangle-reload [] (: (vim.system [:make] {:cwd conf-dir}) :wait)
                     (vim.cmd (.. "source " conf-out))
                     (vim.cmd "redraw | echo '󰑓 Tangled and reloaded config'"))
(vim.api.nvim_create_autocmd
    [:BufWritePost]
    {:pattern conf-src
     :group user-group
     :callback tangle-reload})
(vim.cmd.set (.. (.. :packpath^= (vim.fn.stdpath :data)) :/site))
(vim.cmd "packadd nfnl")
(local {: autoload} (require :nfnl.module))
(local core (autoload :nfnl.core))
(var user-opts {})
(set user-opts (core.merge user-opts
  {:conceallevel 2
   :cursorline true
   :fillchars {:vert "│" }
   :laststatus 3
   :listchars {:tab ">-" :eol "↵" :nbsp "␣" :trail "‧" :extends "⟩" :precedes "⟨"}
   :number true
   :scrolloff 13
   :showmode false
   :sidescrolloff 8
   :signcolumn :yes
   :splitbelow true
   :splitright true
   :termguicolors true}))
(set user-opts (core.merge user-opts
  (let [indent 2]
      {:breakindent true
       :expandtab true
       :shiftwidth indent
       :smartindent true
       :softtabstop indent
       :tabstop indent})))
(set user-opts (core.merge user-opts
  {:grepprg "rg --vimgrep"
   :ignorecase true
   :inccommand :split
   :smartcase true}))
(set user-opts (core.merge user-opts
  {:completeopt [:menu :menuone :noinsert]
   :pumheight 10}))
(set user-opts (core.merge user-opts
  {:hidden true
   :timeoutlen 250
   :undofile true
   :updatetime 250
   :clipboard "unnamedplus"}))
(each [k v (pairs user-opts)]
  (tset vim.opt k v))
(core.assoc vim.g :mapleader " "
                  :maplocalleader ",")
(let [t {:<Left> :<C-w>h
         :<Down> :<C-w>j
         :<Up> :<C-w>k
         :<Right> :<C-w>l}]
         (each [k v (pairs t)]
           (vim.keymap.set :n k v)))
(fn toggle-opt [name]
  (var (on off) nil)
  (if (= name :signcolumn) (set (on off) (values :yes :no))
      (set (on off) (values true false)))
  (if (= (. vim.o name) on) (tset vim.o name off) (tset vim.o name on)))	
(vim.keymap.set :n :<Leader>un (fn [] (toggle-opt :number)) {:desc "Line numbers"})
(vim.keymap.set :n :<Leader>uw (fn [] (toggle-opt :list)) {:desc :Whitespace})
(vim.keymap.set :n :<Leader>uc (fn [] (toggle-opt :cursorline)) {:desc :Cursorline})
(vim.keymap.set :n :<Esc> :<Cmd>nohlsearch<CR> {:desc "Stop highlighting matches"})
(vim.keymap.set :t :<Esc><Esc> :<C-\\><C-n> {:desc "Exit Terminal mode"})
(let [lazypath (.. (vim.fn.stdpath :data) :/lazy/lazy.nvim)
      lazyrepo "https://github.com/folke/lazy.nvim.git"]
  (when (not ((. (or vim.uv vim.loop) :fs_stat) lazypath))
    (let [args [:git :clone "--filter=blob:none" :--branch=stable lazyrepo lazypath]
          out (vim.fn.system args)]
        (when (not= vim.v.shell_error 0)
          (vim.api.nvim_echo [["Failed to clone lazy.nvim:\n" :ErrorMsg]
                              [out :WarningMsg]
                              ["\nPress any key to exit..."]]
                             true {})
          (vim.fn.getchar)
          (os.exit 1))))
  (vim.opt.rtp:prepend lazypath))
(let [lazy (require :lazy)]
    (lazy.setup {:spec [
                   {1 :Olical/nfnl :ft :fennel}
                   {1 :nvim-treesitter/nvim-treesitter
                    :config (fn []
                       (let [ts (require :nvim-treesitter.configs)]
                           (ts.setup {:highlight {:enable true}
                                      :indent {:enable true}
                                      :ensure_installed [:bash :css :elixir :fennel :gdscript :go
                                                         :graphql :html :http :javascript :json :kdl
                                                         :lua :markdown :nix :rust :scss :sql :svelte
                                                         :typescript :xml :yaml]})))}
                   {1 :Olical/conjure}
                   {1 :neovim/nvim-lspconfig
                    :opts {:servers {}}
                    :config (fn [_ opts])}
                 ]
                 :checker {:enabled true}}))
