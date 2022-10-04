(import-macros {: map!} :themis.keybind)

(local tree (require "nvim-tree"))

(tree.setup {:hijack_directories {:enable false}
             :renderer {:icons {:glyphs {:git {:unstaged ""
                                               :staged    ""
                                               :unmerged  ""
                                               :renamed   ""
                                               :untracked ""}}}}})

(map! [n] "<Leader>ee" "<Cmd>NvimTreeToggle<CR>")
(map! [n] "<Leader>ef" "<Cmd>NvimTreeFindFile<CR>")
