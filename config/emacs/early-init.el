;; GNU Emacs early initialization

(setopt frame-resize-pixelwise t
        window-resize-pixelwise t
        ;; frame-inhibit-implied-resize t
        ;; frame-title-format '("%b")
        ring-bell-function 'ignore
        ;; use-dialog-box t ; only for mouse events, which I seldom use
        ;; use-file-dialog nil
        use-short-answers t
        inhibit-splash-screen t
        inhibit-startup-screen t
        ;; inhibit-x-resources t
        ;; inhibit-startup-echo-area-message user-login-name ; read the docstring
        ;; inhibit-startup-buffer-menu t
        )

;; (message-kill-buffer-query nil "Don't ask to confirm buffer kill")

(scroll-bar-mode -1)
(tool-bar-mode -1)

(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(if (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mac-right-option-modifier 'alt))
