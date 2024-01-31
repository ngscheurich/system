;; -*- lexical-binding: t -*-

(use-package typescript-ts-mode
  :elpaca nil
  :init
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode "typescript-language-server" "--stdio"))
  :hook (typescript-ts-mode . eglot-ensure)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)))

(provide 'ngs-typescript)
