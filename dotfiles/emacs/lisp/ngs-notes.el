;; Note-taking helpers -*- lexical-binding: t -*-

;; Simple note taking
;; https://protesilaos.com/emacs/denote
(use-package denote
  :init
  (setq denote-directory (expand-file-name "~/Notes/")
        denote-known-keywords '("emacs" "idea" "person" "followup")
        denote-file-type 'markdown-yaml
        denote-date-prompt-use-org-read-date t)
  :config
  (keymap-global-set "C-c n n" 'denote)
  (keymap-global-set "C-c n f" 'denote-open-or-create))
