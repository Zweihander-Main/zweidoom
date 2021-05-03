;; packages.el --- Located at $DOOMDIR/packages.el
;;; Commentary:
;; -*- no-byte-compile: t; -*-
;; -*- lexical-binding: t; -*-

;;; Code:
(package! org-roam-server)
(package! org-ql)
(package! anki-editor)
(package! format-all)
(package! org-clock-convenience)
(package! org-variable-pitch
  :recipe '(:local-repo "packages/org-variable-pitch"))
(package! org-habit-plus
  :recipe '(:host github :repo "oddious/org-habit-plus"))
(package! vimrc-mode)
(package! kindle-highlights-to-org
  :recipe '(:host github :repo "Zweihander-Main/kindle-highlights-to-org"))

;;; packages.el ends here
