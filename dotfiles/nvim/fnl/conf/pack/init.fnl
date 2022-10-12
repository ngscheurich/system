(import-macros {: pack
                : pack! : unpack!} :themis.pack)

;; ----------------------------------------------------------------
;; Configuration
;; ----------------------------------------------------------------
(pack! "rktjmp/hotpot.nvim")
(pack! "wbthomason/packer.nvim")
(pack! "ngscheurich/themis.nvim")

;; ----------------------------------------------------------------
;; Editing
;; ----------------------------------------------------------------
(pack! "eraserhd/parinfer-rust" {:run "nix shell nixpkgs#cargo -c cargo build --release"
                                 :ft conf.lisp-filetypes})
(pack! "numToStr/Comment.nvim" {:setup* "Comment"})
(pack! "junegunn/vim-easy-align")
(pack! "ngscheurich/edeex.nvim" {:config #((. (require :edeex) :setup) {:mapping "<C-c>e"})})
(pack! "tpope/vim-repeat")
(pack! "tpope/vim-sexp-mappings-for-regular-people")
(pack! "tpope/vim-speeddating")
(pack! "tpope/vim-surround")
(pack! "windwp/nvim-autopairs" {:require* :conf.pack.autopairs})

;; ----------------------------------------------------------------
;; Interface
;; ----------------------------------------------------------------
(pack! "dstein64/vim-win")
(pack! "feline-nvim/feline.nvim" {:require* :conf.pack.feline})
(pack! "folke/todo-comments.nvim" {:requires ["nvim-lua/plenary.nvim"]})
(pack! "folke/which-key.nvim" {:setup* "which-key"})
(pack! "https://gitlab.com/yorickpeterse/nvim-pqf.git" {:setup* "pqf"})
(pack! "kyazdani42/nvim-web-devicons" {:require* :conf.pack.web-devicons})
(pack! "norcalli/nvim-colorizer.lua" {:setup* "colorizer"})
(pack! "rcarriga/nvim-notify")
(pack! "sidebar-nvim/sidebar.nvim" {:require* :conf.pack.sidebar})
(pack! "stevearc/dressing.nvim" {:require* :conf.pack.dressing})

(pack! "EdenEast/nightfox.nvim")
(pack! "RRethy/nvim-base16")
(pack! "folke/tokyonight.nvim")
(pack! "habamax/vim-alchemist")
(pack! "jan-warchol/selenized" {:run "touch post-run"})
(pack! "navarasu/onedark.nvim")
(pack! "romainl/Apprentice")

;; ----------------------------------------------------------------
;; Navigation
;; ----------------------------------------------------------------
(pack! "andymass/vim-matchup")
(pack! "folke/trouble.nvim" {:cmd "Trouble"
                             :setup* "trouble"})
(pack! "ggandor/leap.nvim" {:config #((. (require :leap) :add_default_mappings))})
(pack! "justinmk/vim-dirvish")
(pack! "kyazdani42/nvim-tree.lua" {:require* :conf.pack.tree})
(pack! "nvim-telescope/telescope.nvim" {:requires ["nvim-lua/plenary.nvim"]
                                        :require* :conf.pack.telescope})
(pack! "nvim-telescope/telescope-fzy-native.nvim" {:after "telescope.nvim"
                                                   :run "git submodule update --init --recursive"})

;; ----------------------------------------------------------------
;; Languages
;; ----------------------------------------------------------------
(pack! "Glench/Vim-Jinja2-Syntax")
(pack! "HerringtonDarkholme/yats.vim")
(pack! "LnL7/vim-nix")
(pack! "MaxMEllon/vim-jsx-pretty")
(pack! "bakpakin/fennel.vim")
(pack! "cespare/vim-toml")
(pack! "dag/vim-fish")
(pack! "elixir-editors/vim-elixir")
(pack! "ericpruitt/tmux.vim")
(pack! "euclidianAce/BetterLua.vim")
(pack! "fladson/vim-kitty")
(pack! "hashivim/vim-terraform")
(pack! "ixru/nvim-markdown")
(pack! "jparise/vim-graphql")
(pack! "othree/html5.vim")
(pack! "pangloss/vim-javascript")
(pack! "rust-lang/rust.vim")

;; ----------------------------------------------------------------
;; Code Intelligence
;; ----------------------------------------------------------------
(pack! "folke/lua-dev.nvim")
(pack! "jose-elias-alvarez/null-ls.nvim")
(pack! "neovim/nvim-lspconfig")
(pack! "nvim-treesitter/nvim-treesitter" {:run ":TSUpdate"
                                          :require* :conf.pack.treesitter})
(pack! "nvim-treesitter/playground" {:cmd "TSPlaygroundToggle"})
(pack! "williamboman/mason.nvim" {:setup* "mason"})
(pack! "williamboman/mason-lspconfig.nvim")

;; ----------------------------------------------------------------
;; Completion & Snippets
;; ----------------------------------------------------------------
(pack! :L3MON4D3/LuaSnip {:require* :conf.pack.luasnip})
(pack! "PaterJason/cmp-conjure")
(pack! "hrsh7th/cmp-buffer")
(pack! "hrsh7th/cmp-nvim-lsp")
(pack! "hrsh7th/nvim-cmp")
(pack! "saadparwaiz1/cmp_luasnip")

;; ----------------------------------------------------------------
;; Source Control
;; ----------------------------------------------------------------
(pack! "lewis6991/gitsigns.nvim" {:requires [:nvim-lua/plenary.nvim]
                                  :require* :conf.pack.gitsigns})
(pack! "tpope/vim-fugitive")

;; ----------------------------------------------------------------
;; Notes & Prose
;; ----------------------------------------------------------------
(pack! "folke/zen-mode.nvim" {:require* :conf.pack.zen-mode})
(pack! "iamcco/markdown-preview.nvim" {:run #(vim.fn.mkdp#util#install)
                                       :ft [:markdown]})
(pack! "reedes/vim-pencil" {:ft [:markdown]})

;; ----------------------------------------------------------------
;; Miscelleous Tools
;; ----------------------------------------------------------------
(pack! "Olical/conjure" {:ft conf.lisp-filetypes})
(pack! "janko/vim-test" {:require* :conf.pack.test})
(pack! "michaelb/sniprun" {:run "nix shell nixpkgs#cargo -c bash install.sh"})
(pack! "mbbill/undotree")
(pack! "tpope/vim-abolish")
(pack! "tpope/vim-dispatch")
(pack! "tpope/vim-eunuch")
(pack! "tpope/vim-projectionist")
(pack! "tpope/vim-rsi")
(pack! "tpope/vim-sleuth")
(pack! "tpope/vim-unimpaired")

(unpack!)
