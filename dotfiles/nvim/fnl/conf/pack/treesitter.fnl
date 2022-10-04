(import-macros {: map!} :themis.keybind)

(local treesitter (require :nvim-treesitter.configs))

(local langs [:c
              :c_sharp
              :css
              :dockerfile
              :elixir
              :erlang
              :fennel
              :gdscript
              :graphql
              :http
              :javascript
              :json
              :lua
              :nix
              :python
              :regex
              :ruby
              :toml
              :typescript
              :yaml])

(treesitter.setup
  {:ensure_installed langs
   :highlight {:enable true :disable [:elixir]}
   :incremental_selection {:enable true} 
   :indent {:enable true}
   :rainbow {:enable true :disable [:elixir]}})

(map! [n] "<Leader>Ti" "<Cmd>TSToggle indent<CR>")
