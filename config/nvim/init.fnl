;; The Neovim configuration directory
(local conf-dir :/private/etc/system/config/nvim)

;; The literate config file
(local conf-src (.. conf-dir :/README.md))

;; The tangled/compiled config file
(local conf-out (.. conf-dir :/init.lua))

;; An autocommand group to contain our autocommands
;; See `:h augroup`
(local user-group (vim.api.nvim_create_augroup :ngs {}))
(fn tangle-reload [] (: (vim.system [:make] {:cwd conf-dir}) :wait)
                     (vim.cmd (.. "source " conf-out))
                     (vim.cmd "redraw")
                     (vim.notify "󰑓 Tangled and reloaded config"))
(vim.api.nvim_create_autocmd
    [:BufWritePost]
    {:pattern conf-src :group user-group :callback tangle-reload})
(vim.cmd.set (.. (.. :packpath^= (vim.fn.stdpath :data)) :/site))
(vim.cmd "packadd nfnl")
(local {: assoc
        : get-in
        : merge
        : reduce
        : some} (require :nfnl.core))
(each [k v (pairs {
            :conceallevel 2
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
            :termguicolors true
            :breakindent true
            :expandtab true
            :shiftwidth 2
            :smartindent true
            :softtabstop 2
            :tabstop 2
            :grepprg "rg --vimgrep"
            :ignorecase true
            :inccommand :split
            :smartcase true
            :completeopt [:menu :menuone :noinsert]
            :pumheight 10
            :hidden true
            :timeoutlen 250
            :undofile true
            :updatetime 250
            :clipboard :unnamedplus
            })]
  (tset vim.opt k v))
(fn nmap [lhs rhs opt] (vim.keymap.set :n lhs rhs opt))
(fn imap [lhs rhs opt] (vim.keymap.set :i lhs rhs opt))
(fn tmap [lhs rhs opt] (vim.keymap.set :t lhs rhs opt))
(let [t {:<Left> :<C-w>h
         :<Down> :<C-w>j
         :<Up> :<C-w>k
         :<Right> :<C-w>l}]
         (each [k v (pairs t)]
           (nmap k v)))
(fn toggle-opt [name]
  (var (on off) nil)
  (if (= name :signcolumn) (set (on off) (values :yes :no))
      (set (on off) (values true false)))
  (if (= (. vim.o name) on) (tset vim.o name off) (tset vim.o name on)))	
(nmap :<Leader>un (fn [] (toggle-opt :number)) {:desc "Line numbers"})
(nmap :<Leader>uw (fn [] (toggle-opt :list)) {:desc :Whitespace})
(nmap :<Leader>uc (fn [] (toggle-opt :cursorline)) {:desc :Cursorline})
(nmap :<Esc> :<Cmd>nohlsearch<CR> {:desc "Stop highlighting matches"})
(tmap :<Esc><Esc> :<C-\><C-n> {:desc "Exit Terminal mode"})
(assoc vim.g :mapleader " "
             :maplocalleader ",")
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
(fn spec [plugin tbl]
  "Returns `plugin` if `tbl` is `nil`. Otherwise, returns `tbl` with `plugin`
  as the value associated with the `[1]` key."
  (case tbl
    nil {1 plugin}
    _ (assoc tbl 1 plugin)))
(fn lazy-key [desc lhs rhs]
  {1 lhs 2 rhs :desc desc})
(let [lazy (require :lazy)]
  (lazy.setup {:checker {:enabled true}
               :dev {:path "~/Projects"}
               :install {:colorscheme [:catppuccin]}
               :performance {:rtp {:disabled_plugins [:gzip :netrwPlugin :tarPlugin
                                                      :tohtml :tutor :zipPlugin]}}
               :spec [
                 (spec :Olical/nfnl {:ft :fennel})
                 (spec :folke/snacks.nvim
                       {:priority 1000
                        :lazy false
                        :opts {
                        :utils {}
                        :bigfile {}
                        :quickfile {}
                        :scratch {}
                        :image {}
                        :picker {}
                        :notifier {}
                        :scroll {}
                        :statuscolumn {}
                        :input {}
                        :indent {:enabled false :only_scope true :only_current true}
                        :gitbrowse {}
                        }
                        :keys [
                        ;; Quick
                        (lazy-key "Files" :<C-f> #(Snacks.picker.git_files {:layout :ivy}))
                        (lazy-key "Grep" :<C-g> #(Snacks.picker.grep {:layout :ivy}))
                        (lazy-key "Lines" :<C-_> #(Snacks.picker.lines {:layout :ivy}))
                        (lazy-key "Buffers" :<C-Space> #(Snacks.picker.buffers {:layout :select}))

                        ;; General
                        (lazy-key "Find Files (Smart)" :<Leader><Leader> #(Snacks.picker.smart {:layout :ivy}))
                        (lazy-key "Resume Picker" :<Leader>r #(Snacks.picker.resume))

                        ;; Find
                        (lazy-key "Files" :<Leader>ff #(Snacks.picker.files))
                        (lazy-key "Git Files" :<Leader>fg #(Snacks.picker.git_files))
                        (lazy-key "Projects" :<Leader>fp #(Snacks.picker.projects))
                        (lazy-key "Recent Files" :<Leader>fr #(Snacks.picker.recent))

                        ;; Search
                        (lazy-key "Autocommands" :<Leader>sa #(Snacks.picker.autocmds))
                        (lazy-key "Buffers" :<Leader>sB #(Snacks.picker.grep_buffers))
                        (lazy-key "Command History" "<Leader>s:" #(Snacks.picker.command_history))
                        (lazy-key "Commands" :<Leader>sc #(Snacks.picker.commands))
                        (lazy-key "Diagnostics" :<Leader>sd #(Snacks.picker.diagnostics))
                        (lazy-key "Diagnostics (Buffer)" :<Leader>sD #(Snacks.picker.diagnostics_buffer))
                        (lazy-key "Grep" :<Leader>sg #(Snacks.picker.grep))
                        (lazy-key "Help Pages" :<Leader>sh #(Snacks.picker.help))
                        (lazy-key "Highlights" :<Leader>sH #(Snacks.picker.highlights))
                        (lazy-key "Icons" :<Leader>si #(Snacks.picker.icons))
                        (lazy-key "Jumps" :<Leader>sj #(Snacks.picker.jumps))
                        (lazy-key "Keymaps" :<Leader>sk #(Snacks.picker.keymaps))
                        (lazy-key "Lines (Buffer)" :<Leader>sb #(Snacks.picker.lines))
                        (lazy-key "Location List" :<Leader>sl #(Snacks.picker.loclist))
                        (lazy-key "Man Pages" :<Leader>sM #(Snacks.picker.man))
                        (lazy-key "Marks" :<Leader>sm #(Snacks.picker.marks))
                        (lazy-key "Notification History" :<Leader>sn #(Snacks.picker.notifications))
                        (lazy-key "Quickfix List" :<Leader>sq #(Snacks.picker.qflist))
                        (lazy-key "Registers" "<Leader>s\"" #(Snacks.picker.registers))
                        (lazy-key "Search History" :<Leader>s/ #(Snacks.picker.search_history))
                        (lazy-key "Undo History" :<Leader>su #(Snacks.picker.undo))
                        (lazy-key "Word" :<Leader>sw #(Snacks.picker.grep_word))
                        ;; LSP
                        (lazy-key "Definitions" :<LocalLeader>d #(Snacks.picker.lsp_definitions))
                        (lazy-key "Declarations" :<LocalLeader>D #(Snacks.picker.lsp_declarations))
                        (lazy-key "Implementations" :<LocalLeader>i #(Snacks.picker.lsp_implementations))
                        (lazy-key "References" :<LocalLeader>r #(Snacks.picker.lsp_references))
                        (lazy-key "Symbols" :<LocalLeader>s #(Snacks.picker.lsp_symbols))
                        (lazy-key "Workspace Symbols" :<LocalLeader>S #(Snacks.picker.lsp_workspace_symbols))
                        (lazy-key "Type Definitions" :<LocalLeader>t #(Snacks.picker.type_definitions))
                        (lazy-key "Indent Guides"
                                  :<Leader>ui
                                  (fn []
                                    (if Snacks.indent.enabled
                                      (Snacks.indent.disable)
                                      (Snacks.indent.enable))))
                        (lazy-key "View on Remote" :<Leader>gB #(Snacks.gitbrowse))
                        ]})
                 (spec :ggandor/leap.nvim
                       {:config (fn []
                                  (let [leap (require :leap)]
                                    (leap.add_default_mappings)))})
                 (spec :echasnovski/mini.align {:version :* :config true})
                 (spec :echasnovski/mini.pairs
                       {:version :*
                        :opts {:skip_ts [:string]}})
                 (spec :echasnovski/mini.splitjoin {:version :* :config true})
                 (spec :echasnovski/mini.surround
                       {:version :*
                        :opts {:mappings {:add :\a
                                          :delete :\d
                                          :find :\f
                                          :find_left :\F
                                          :highlight :\h
                                          :replace :\r
                                          :update_n_lines :\n}}})
                 (spec :stevearc/aerial.nvim
                       {:config (fn []
                                  (let [{: setup} (require :aerial)]
                                    (setup {:on_attach (fn [b]
                                                         (nmap "{" :<Cmd>AerialPrev<CR> {:buffer b})
                                                         (nmap "}" :<Cmd>AerialNext<CR> {:buffer b}))})))
                       :keys [(lazy-key :Outline :<Leader>o :<Cmd>AerialToggle!<CR>)]})
                 (spec :nvim-neo-tree/neo-tree.nvim
                       {:branch :v3.x
                        :dependencies [:nvim-lua/plenary.nvim :MunifTanjim/nui.nvim]
                        :keys [(lazy-key :Explore :<Leader>e "<Cmd>Neotree reveal<CR>")]})
                 (spec :stevearc/oil.nvim
                       {:config (fn []
                                  (let [oil (require :oil)]
                                    (var detail false)

                                    (fn toggle-detail []
                                      (set detail (not detail))
                                      (if detail
                                          (oil.set_columns [:icon :permissions :size :mtime])
                                          (oil.set_columns [:icon])))

                                    (oil.setup {:default_file_explorer true
                                                :keymaps {:gd {:desc "Toggle detail view"
                                                               :callback toggle-detail}}})))
                        :keys [(lazy-key "Open parent directory" :- :<Cmd>Oil<CR>)]})
                 (spec :echasnovski/mini.bracketed {:version :* :config true})
                 (spec :echasnovski/mini.clue
                       {:version :*
                       :opts {:triggers  [{:mode :n :keys :<Leader>}
                                          {:mode :n :keys :g}
                                          {:mode :x :keys :g}
                                          {:mode :n :keys :z}
                                          {:mode :x :keys :z}]
                             :clues [{:mode :n :keys :<Leader>f :desc :+Find}
                                     {:mode :n :keys :<Leader>g :desc :+Git}
                                     {:mode :n :keys :<Leader>s :desc :+Search}
                                     {:mode :n :keys :<Leader>S :desc :+Sessions}
                                     {:mode :n :keys :<Leader>t :desc :+Test}
                                     {:mode :n :keys :<Leader>u :desc "+UI Toggles"}]}})
                 (spec :echasnovski/mini.sessions
                       {:version :*
                       :config (fn []
                                 (let [sessions (require :mini.sessions)]
                                   (sessions.setup)

                                   (fn session-name []
                                     (local n (string.gsub (vim.fn.getcwd) :/ :_))
                                     n)

                                   (nmap :<Leader>Sw #(sessions.write (session-name)) {:desc :Write})
                                   (nmap :<Leader>Sr #(sessions.read (session-name)) {:desc :Read})
                                   (nmap :<Leader>Ss #(sessions.select) {:desc :Read}))) })
                 (spec :tpope/vim-rsi)
                 (spec :jpalardy/vim-slime {:config #(tset vim.g :slime_target :tmux)})
                 (spec :tpope/vim-projectionist)
                 (spec :nvim-treesitter/nvim-treesitter
                       {:opts {:highlight {:enable true}
                               :indent {:enable true}
                               :ensure_installed [
                               ;; Tree-sitter parsers
                               :bash
                               :css
                               :elixir
                               :erlang
                               :gdscript
                               :go
                               :graphql
                               :heex
                               :html
                               :http
                               :javascript
                               :json
                               :kdl
                               :lua
                               :nix
                               :rust
                               :scss
                               :sql
                               :svelte
                               :typescript
                               :xml
                               :yaml
                               ]}
                        :config (fn [_ opts]
                                  (let [{: setup} (require :nvim-treesitter.configs)]
                                    (setup opts)))})
                  (spec :neovim/nvim-lspconfig
                        {:config (fn []
                                   (let [lc (require :lspconfig)
                                         servers {
                                         ;; LSP servers
                                         :bashls {}
                                         :lexical {:cmd [(.. vim.env.HOME "/Projects/lexical/_build/dev/package/lexical/bin/start_lexical.sh")]}
                                         :gdscript {}
                                         :gopls {}
                                         :lua_ls {}
                                         :nil_ls {}
                                         :rust_analyzer {}
                                         :ts_ls {}
                                         }]
                                     (each [s c (pairs servers)]
                                         ((get-in lc [s :setup]) c))))})
                 (spec :saghen/blink.cmp
                       {:version :0.12.4
                        :opts {:keymap {:preset :super-tab}
                               :sources {:default [:lsp :path :snippets]}
                               :cmdline {:enabled false}
                               :completion {:ghost_text {:enabled true}
                                            :menu {:border :none
                                                   ;; :auto_show #(not= $1.mode :cmdline)
                                                   :auto_show false
                                                   }
                                            :documentation {:window {:border :single}
                                                            :auto_show true :auto_show_delay_ms 500
                                                           }}}
                        :config (fn [_ opts]
                                  (let [cmp (require :blink.cmp)]
                                    (cmp.setup opts)
                                    (imap :<C-n> #(cmp.show {:providers [:buffer]}))))})
                 (spec :catppuccin/nvim
                       {:name :catppuccin
                        :lazy false
                        :priority 1000
                        :config (fn []
                                  (let [{: setup} (require :catppuccin)]
                                    (setup {:color_overrides {:mocha {:rosewater :#ab7e8a
                                                                      :flamingo  :#a3685a
                                                                      :pink      :#b294bb
                                                                      :mauve     :#c07d90
                                                                      :red       :#cc6566
                                                                      :maroon    :#d57d62
                                                                      :peach     :#de935f
                                                                      :yellow    :#f0c674
                                                                      :green     :#b6bd68
                                                                      :teal      :#9fbd8f
                                                                      :sky       :#8abeb7
                                                                      :sapphire  :#85b0bc
                                                                      :blue      :#82a2be
                                                                      :lavender  :#a3a7c2
                                                                      :text      :#c4c8c6
                                                                      :subtext1  :#b5b7b4
                                                                      :subtext0  :#969896
                                                                      :overlay2  :#838585
                                                                      :overlay1  :#717374
                                                                      :overlay0  :#5e6063
                                                                      :surface2  :#4a4e52
                                                                      :surface1  :#373b41
                                                                      :surface0  :#282a2e
                                                                      :base      :#1d1f21
                                                                      :mantle    :#151718
                                                                      :crust     :#0e0f10}}
                                                              :integrations {:aerial true
                                                                             :blink_cmp true}}))
                                  (vim.cmd.colorscheme :catppuccin))})
                 (spec :Bekaboo/dropbar.nvim
                       {:opts {:bar {:enable false}}
                        :keys [(lazy-key :Breadcrumbs
                                         :<Leader>ub
                                         (fn []
                                           (if (= vim.o.winbar "")
                                               (tset vim.o :winbar "%{%v:lua.dropbar()%}")
                                               (tset vim.o :winbar ""))))]
                        :config true})
                 (spec :echasnovski/mini.icons {:version :* :config true :lazy false})
                 (spec :catgoose/nvim-colorizer.lua
                       {:event :BufReadPre
                        :opts {:user_default_options {:names false}}})
                 ;; TODO: Some of these settings don't seem to work...
                 ;(spec :rachartier/tiny-glimmer.nvim
                 ;      {:opts {:overwrite {:search {:enabled true}
                 ;                          :undo {:enabled true}
                 ;                          :redo {:enabled true}}}})
                 (spec :petertriho/nvim-scrollbar {:config true})
                 (spec :j-hui/fidget.nvim {:config true :event :LspProgress})
                 (spec :b0o/incline.nvim {:config true})
                 (spec :sphamba/smear-cursor.nvim {:config true})
                 (spec :akinsho/bufferline.nvim
                       {:version :*
                        :after :catppuccin
                        :event [:TabEnter :TabNew :TabNewEntered]
                        :opts {:options {:mode :tabs
                                         :indicator {:icon "┃ "}
                                         :always_show_bufferline false
                                         :offsets [{:filetype :neo-tree
                                                    :text " Explorer"
                                                    :text_align :left
                                                    :separator false}]}}})
                 (spec :lewis6991/gitsigns.nvim
                       {:config (fn []
                                  (let [gs (require :gitsigns)]
                                    (gs.setup {:signs {:add {:text :┃}
                                                           :change {:text :┃}
                                                           :changedelete {:text :┃}
                                                           :delete {:text :┃}
                                                           :topdelete {:text :┃}
                                                           :untracked {:text :┇}}})
                                    (nmap :<Leader>gb gs.toggle_current_line_blame {:desc "Line blame (toggle)"})
                                    (nmap :<Leader>gd gs.toggle_deleted {:desc "Deleted (toggle)"})
                                    (nmap :<Leader>gh gs.toggle_linehl {:desc "Line highlight (toggle)"})
                                    (nmap :<Leader>gp gs.preview_hunk {:desc "Preview hunk"})
                                    (nmap :<Leader>gp gs.reset_hunk {:desc "Reset hunk"})
                                    (nmap "[h" gs.prev_hunk {:desc "Previous hunk"})
                                    (nmap "]h" gs.next_hunk {:desc "Next hunk"})))})
                 (spec :rebelot/heirline.nvim)
                 (spec :stevearc/conform.nvim
                       {:opts {:formatters_by_ft {
                               :css [:prettier]
                               :html [:prettier]
                               :javascript [:prettier]
                               :json [:prettier]
                               :lua [:stylua]
                               :scss [:prettier]
                               :sql [:sleek]
                               :typescript [:prettier]
                               }
                               :format_on_save {:timeout_ms 500
                                                :lsp_fallback true}}})
                 (spec :mfussenegger/nvim-lint
                       {:config (fn []
                                  (let [lint (require :lint)]
                                    (tset lint :linters_by_ft {:sh [:shellcheck]
                                                               :sql [:sqlfluff]})))})
                 (spec :OXY2DEV/markview.nvim
                       {:ft [:markdown :avante]
                        :config (fn []
                                  (let [editor (require :markview.extras.editor)]
                                    (editor.setup)))})
                 (spec :zbirenbaum/copilot.lua
                       {:cond false
                        :cmd :Copilot
                        :event :InsertEnter
                        :opts {:suggestion {:keymap {:accept :<Tab>
                                                     :next "<C-n>"
                                                     :prev "<C-p>"
                                                     :dismiss "<C-q>"}}}})
                 (spec :olimorris/codecompanion.nvim
                       {:cond false
                        :config true 
                        :dependencies [:nvim-lua/plenary.nvim
                                       :nvim-treesitter/nvim-treesitter]
                        :strategies {:chat {:adapter :anthropic}
                                     :inline {:adapter :anthropic}}})
                 (spec :yetone/avante.nvim
                       {:cond false
                        :event :VeryLazy
                        :version false
                        :build :make
                        :opts {:windows {:sidebar_header {:rounded false}
                                         :input {:prefix " "}}}
                        :dependencies [:nvim-lua/plenary.nvim
                                       :nvim-treesitter/nvim-treesitter
                                       :MunifTanjim/nui.nvim]})
                 (spec :mistweaverco/kulala.nvim
                   {:ft [:http :rest]
                    :opts {:global_keymaps true}})

                 (spec :Olical/conjure
                       {:config (fn []
                                 (set vim.g.conjure#client#fennel#aniseed#deprecation_warning false))})

                 ;; {1 :iamcco/markdown-preview.nvim
                 ;;  :build (fn [] ((. vim.fn "mkdp#util#install")))
                 ;;  :cmd [:MarkdownPreviewToggle :MarkdownPreview :MarkdownPreviewStop]
                 ;;  :ft [:markdown]}

                 (spec :brianhuster/live-preview.nvim)

                 (spec :sindrets/diffview.nvim {:config true})

                 ;; (spec :joshuavial/aider.nvim {:opts {}})

                 (spec :kristijanhusak/vim-dadbod-ui
                       {:dependencies [{1 :tpope/vim-dadbod :lazy true}
                                       {1 :kristijanhusak/vim-dadbod-completion :ft [:sql :mysql :plsql] :lazy true}]
                        :cmd [:DBUI :DBUIToggle :DBUIAddConnection :DBUIFindBuffer]
                        :init (fn [] (tset vim.g :db_ui_use_nerd_fonts 1))})

                 (spec :nvim-neotest/neotest
                       {:dependencies [:nvim-neotest/nvim-nio
                                       :antoinemadec/FixCursorHold.nvim
                                       :jfpedroza/neotest-elixir]
                        :config (fn []
                                  (let [{: setup} (require :neotest)]
                                    (setup {:adapters [(require :neotest-elixir)]})))
                        ;:ft [:elixir]
                        :keys [(lazy-key :Nearest :<Leader>tn #((get-in (require :neotest) [:run :run])))
                               (lazy-key :Last :<Leader>tt #((get-in (require :neotest) [:run :run_last])))
                               (lazy-key :File :<Leader>tf #((get-in (require :neotest) [:run :run]) (vim.fn.expand :%)))
                               (lazy-key "Summary (Toggle)" :<Leader>ts #((get-in (require :neotest) [:summary :toggle])))]})

                 (spec :folke/trouble.nvim
                       {:cmd [:Trouble]
                        :config true
                        :keys [(lazy-key "Diagnostics" :<Leader>ld "<Cmd>Trouble diagnostics toggle filter.buf=0<CR>")
                               (lazy-key "Diagnostics (Workspace)" :<Leader>lD "<Cmd>Trouble diagnostics toggle<CR>")
                               (lazy-key "Symbols" :<Leader>ls "<Cmd>Trouble symbols toggle<CR>")
                               (lazy-key "LSP" :<Leader>lx "<Cmd>Trouble lsp toggle<CR>")
                               (lazy-key "Location List" :<Leader>ll "<Cmd>Trouble loclist toggle<CR>")
                               (lazy-key "Quickfix List" :<Leader>lq "<Cmd>Trouble qflist toggle<CR>")]})
               ]
               }))
(vim.cmd "so ~/.config/nvim/scratch.lua")
