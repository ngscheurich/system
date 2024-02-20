;; Note-taking helpers -*- lexical-binding: t -*-

;; Simple note taking
;; https://protesilaos.com/emacs/denote
(use-package denote
  :init
  (setq denote-directory (expand-file-name "~/Notes/")
        denote-known-keywords '("emacs" "idea" "person" "followup")
        denote-file-type 'markdown-yaml
        denote-date-prompt-use-org-read-date t)

  :bind
  (("C-c n n" . 'denote)
   ("C-c n f"  . 'denote-open-or-create)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(provide 'ngs-notes)
