(import-macros {: augroup!
                : clear!
                : autocmd!} :themis.event)

(augroup! yank-highlight
          (clear!)
          (autocmd! TextYankPost * '(vim.highlight.on_yank)))
