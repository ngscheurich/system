(import-macros {: map!} :themis.keybind)

(local sidebar (require "sidebar-nvim"))

(sidebar.setup {:side :right
                :section_separator ""
                :sections [:git :diagnostics :todos :files :buffers]})

(map! [n] "<Leader>Ts" "<Cmd>SidebarNvimToggle<CR>")
