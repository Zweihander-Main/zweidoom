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
  :recipe '(:local-repo "lisp/org-variable-pitch"))
(package! org-habit-plus
  :recipe '(:host github :repo "oddious/org-habit-plus"))

;;; packages.el ends here
