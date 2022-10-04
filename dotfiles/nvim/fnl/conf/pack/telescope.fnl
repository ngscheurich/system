(import-macros {: map!} :themis.keybind)

(local {:find_files find-files
        :live_grep live-grep
        :buffers buffers
        :help_tags help-tags
        :quickfix quickfix
        :loclist loclist} (require :telescope.builtin))

(map! [n] "<Leader>ff" find-files)
(map! [n] "<Leader>fg" live-grep)
(map! [n] "<Leader>fb" buffers)
(map! [n] "<Leader>fh" help-tags)
(map! [n] "<Leader>fq" quickfix)
(map! [n] "<Leader>fk" loclist)
