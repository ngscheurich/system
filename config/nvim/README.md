---
title: Nick’s Literate Neovim Config
---

# Neovim Configuration

- [Process](#process)
- [Initialization](#initialization)
- [Prelude](#prelude)
- [Core](#core)
- [Keymaps](#keymaps)
- [Plugins](#plugins)

This is my [Neovim] configuration. It is written in a literate programming style; that is to say, with text meant for computer interpretation, i.e. _code_, intermingled with plain-language text meant to be parsed by humans. Specifically, this file is a [Markdown] document in which the computer code portions are marked up as [fenced code blocks] written in the [Fennel] programming language.

## Process

From a technical perspective, when this file is written to disk, each code block is exported to a Fennel file. These Fennel files are compiled into [Lua] code, which is interpreted by Neovim when it starts.

The process of exporting embedded code blocks is oftentimes referred to as _tangling_, and is handled in this case by [Literate Markdown Tangle] (LMT).

Compiling the tangled Fennel into Lua code is the responsibility of [nfnl], an excellent project by [Oliver Caldwell] which also provides access to a [Clojure]-inspired standard library.

## Initialization

LMT and nfnl can be installed by running `make init`. This command assumes the following tools are available on the system:

- [Make]
- [Git]
- [Go]

LMT is cloned from GitHub into a temporary directory and built from source. nfnl is cloned from GitHub into a directory compatible with the Vim packages feature (`:h packages`).

nfnl expects a configuration file named `.nfnl.fnl` in this directory:

```sh .nfnl.fnl +=
{}
```

`make init` also performs the first tangle and compile steps, as described [above](#process). At this point you should see compiled Lua files!

## Prelude

With that, I’m ready to begin configuring Neovim. First things first: I’ll set up some generally useful variables.

```fennel init.fnl +=
;; The Neovim configuration directory
(local conf-dir (vim.fn.stdpath :config))

;; The literate config file
(local conf-src (.. conf-dir :/README.md))

;; The tangled/compiled config file
(local conf-out (.. conf-dir :/init.lua))

;; An autocommand group to contain our autocommands
;; See `:h augroup`
(local user-group (vim.api.nvim_create_augroup :ngs {}))
```

The following is a function that will synchronously tangle and compile, then _source_ (`:h source`) the results. Sourcing a file will cause Neovim to run the code contained therein, effectively reloading the connfig.

```fennel init.fnl +=
(fn tangle-reload [] (: (vim.system [:make] {:cwd conf-dir}) :wait)
                     (vim.cmd (.. "source " conf-out))
                     (vim.cmd "redraw | echo '󰑓 Tangled and reloaded config'"))
```

> [!TIP]
> The 󰑓 character above may not render correctly without access to certain font symbols; this will not affect functionality. 

Next I’ll create an autocommand that executes the `tangle-reload` function after the buffer representing this file is written. This gives us the effect of the Neovim session applying changes as they are made in this file!

```fennel init.fnl +=
(vim.api.nvim_create_autocmd
    [:BufWritePost]
    {:pattern conf-src
     :group user-group
     :callback tangle-reload})
```

I also want access to some nfnl niceties, namely the `autoload` function and the standard (`core`) library.

```fennel init.fnl +=
(vim.cmd.set (.. (.. :packpath^= (vim.fn.stdpath :data)) :/site))
(vim.cmd "packadd nfnl")
(local {: autoload} (require :nfnl.module))
(local core (autoload :nfnl.core))
```

`autoload` here works a bit like VimL’s autoloading behavior (`:h autoload`) in that it will not `require` a module until it is used.

# Core

In this section, I’ll configure built-in Neovim options and functionality. Each option is described in the Neovim documentation (`:h [option]`).

```fennel init.fnl +=
(var user-opts {})
```

## Options

First up, I set options that affect the user interface such as turning on line numbers, displaying the sign column, and using a global status line.

```fennel init.fnl +=
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
```

Next up are rules around indentation. Namely, I prefer using two spaces for indentation and for Neovim to intelligently indent new lines when possible.

```fennel init.fnl +=
(set user-opts (core.merge user-opts
  (let [indent 2]
      {:breakindent true
       :expandtab true
       :shiftwidth indent
       :smartindent true
       :softtabstop indent
       :tabstop indent})))
```

When it comes to searching, I prefer to use [ripgrep], ignore case, and preview substition commands in real-time.

```fennel init.fnl +=
(set user-opts (core.merge user-opts
  {:grepprg "rg --vimgrep"
   :ignorecase true
   :inccommand :split
   :smartcase true}))
```

For completion, I set the insert mode completion option (`:h completeopt`) to:

- Use a popup menu to show the possible completions 
- Use the popup menu when even if there is only one match
- Not insert any text until I select a match

Furthermore, I set the popup menu’s height to 10 rows.

```fennel init.fnl +=
(set user-opts (core.merge user-opts
  {:completeopt [:menu :menuone :noinsert]
   :pumheight 10}))
```

Last up is miscellaneous behavior (check out the `:help` docs for each option).

```fennel init.fnl +=
(set user-opts (core.merge user-opts
  {:hidden true
   :timeoutlen 250
   :undofile true
   :updatetime 250
   :clipboard "unnamedplus"}))
```

With all of my configuration options captured in `user-opts` table, I can easily iterate over it and applu each one.

```fennel init.fnl +=
(each [k v (pairs user-opts)]
  (tset vim.opt k v))
```

# Keymaps

While the majority of my custom keymaps are related to plugins, and thereby handled in the [plugins], I so set a few to control native Neovim behavior.

Firstly, my leader key and local leader key are mapped to `<Space>` and `,`, respectively.

```fennel init.fnl +=
(core.assoc vim.g :mapleader " "
                  :maplocalleader ",")
```

I have `Ctrl` plus `h`, `j`, `k`, and `l` mapped to the arrow keys, which become a very ergonomic way to navigate windows.

```fennel init.fnl +=
(let [t {:<Left> :<C-w>h
         :<Down> :<C-w>j
         :<Up> :<C-w>k
         :<Right> :<C-w>l}]
         (each [k v (pairs t)]
           (vim.keymap.set :n k v)))
```

I set some UI toggles, e.g. turning on and off line numbers, under the `<Leader>u` namespace. But first, since some options are toggled differently than others, I’ll create a function to handle that logic.

```fennel init.fnl +=
(fn toggle-opt [name]
  (var (on off) nil)
  (if (= name :signcolumn) (set (on off) (values :yes :no))
      (set (on off) (values true false)))
  (if (= (. vim.o name) on) (tset vim.o name off) (tset vim.o name on)))	
```

Now, for the actual UI toggle keymaps.

```fennel init.fnl +=
(vim.keymap.set :n :<Leader>un (fn [] (toggle-opt :number)) {:desc "Line numbers"})
(vim.keymap.set :n :<Leader>uw (fn [] (toggle-opt :list)) {:desc :Whitespace})
(vim.keymap.set :n :<Leader>uc (fn [] (toggle-opt :cursorline)) {:desc :Cursorline})
```

Finally, some keymaps around the `<Esc>` key:

- Press it once in Normal mode to stop highlighting search matches
- Press it twice in Terminal mode to switch to Normal mode

```fennel init.fnl +=
(vim.keymap.set :n :<Esc> :<Cmd>nohlsearch<CR> {:desc "Stop highlighting matches"})
(vim.keymap.set :t :<Esc><Esc> :<C-\\><C-n> {:desc "Exit Terminal mode"})
```

# Plugins

While the core Neovim experience is excellent, I lean on a bunch of community plugins to elevate it to the next level. I’ll break these plugins up into sections:

- [AI](#ai)
- [Analysis](#analysis)
- [Completion](#completion)
- [Database](#database)
- ...

Vim has a built in plugin management capabilities in its _packages_ feature (`:h packages`). While this is perfect for many cases, I’ve had an excellent experience using the [lazy.nvim] plugin manager. Some of the features that won me over include:

- Versatile deferred, i.e. _lazy_, loading of plugins
- Useful UI for inspecting and managing plugins
- Dependency management
- A lockfile for reproducibility
- Detailed profiling of plugin load times

There’a a bit of bootstrapping required to get lazy.nvim installed.

```fennel init.fnl +=
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
```

Now we can require lazy.nvim and set it up. I’ll use LMT’s [macro references] feature to inject `lazy-spec` into lazy.nvim’s setup function; that way I can build it out bit by bit throughout this section.

```fennel init.fnl +=
(let [lazy (require :lazy)]
    (lazy.setup {:spec [
                   {1 :Olical/nfnl :ft :fennel}
                   <<<lazy-spec>>>
                 ]
                 :checker {:enabled true}}))
```

Now I have access lazy.nvim’s UI via the `:Lazy` command. Time to install some plugins.

## Treesitter

One crucial feature I’m missing is a syntax highlighting for Fennel–the very language I’m writing this config in! Rather than install a bespoke Fennel syntax plugin, I…

```fennel "lazy-spec" +=
{1 :nvim-treesitter/nvim-treesitter
 :config (fn []
    (let [ts (require :nvim-treesitter.configs)]
        (ts.setup {:highlight {:enable true}
                   :indent {:enable true}
                   :ensure_installed [:bash :css :elixir :fennel :gdscript :go
                                      :graphql :html :http :javascript :json :kdl
                                      :lua :markdown :nix :rust :scss :sql :svelte
                                      :typescript :xml :yaml]})))}
```

## Language Server Protocol

Neovim has a robust built-in [Language Server Protocol] (LSP) client, which I set up using the [nvim-lspconfig].

```fennel "lazy-spec" +=
{1 :Olical/conjure}
```

```fennel "lazy-spec" +=
{1 :neovim/nvim-lspconfig
 :opts {:servers {}}
 :config (fn [_ opts])}
```


[autocommand group]: https://neovim.io/doc/user/autocmd.html#_8.-groups
[autocommand]: https://neovim.io/doc/user/autocmd.html#_1.-introduction
[clojure]: https://clojure.org/
[fenced code blocks]: https://spec.commonmark.org/0.31.2/#fenced-code-blocks
[fennel]: https://fennel-lang.org/
[git]: https://git-scm.com/
[go]: https://go.dev/
[language server protocol]: https://microsoft.github.io/language-server-protocol/
[lazy.nvim]: https://github.com/folke/lazy.nvim
[literate markdown tangle]: https://github.com/driusan/lmt
[lua]: https://www.lua.org/
[macro refereneces]: https://github.com/driusan/lmt/blob/master/README.md#macro-references
[make]: https://en.wikipedia.org/wiki/Make_%28software%29
[markdown]: https://spec.commonmark.org/0.31.2/#what-is-markdown-
[neovim]: https://neovim.io/
[nfnl]: https://github.com/Olical/nfnl
[oliver caldwell]: https://github.com/Olical
[plugins]: #pluins
[process]: #process
[ripgrep]: https://github.com/BurntSushi/ripgrep
[vim package]: https://neovim.io/doc/user/repeat.html#_using-vim-packages
