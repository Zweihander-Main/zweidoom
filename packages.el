;; packages.el --- Located at $DOOMDIR/packages.el-*-lexical-binding:t-*-
;;; Commentary:

;;; Code:

;; Unpin doom's version
(unpin! org-roam)

;; Load 3rd party packages
(package! init-loader)
(package! benchmark-init)
(package! org-ref)
(package! websocket) ;; org-roam-ui related
(package! org-roam-ui)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(when (featurep! :completion vertico)
  (package! citar))
(package! org-ql)
(package! anki-editor)
(package! org-clock-convenience)
(package! vimrc-mode)
(package! flycheck-package)
(package! flycheck-eldev)
(package! erefactor)
(package! org-variable-pitch
  :recipe '(:host github
            :repo "cadadr/elisp"
            :files ("org-variable-pitch.el")))
(package! evil-motion-trainer
  :recipe '(:host github :repo "martinbaillie/evil-motion-trainer"))
(package! emacs-with-nyxt
  :recipe '(:host github
            :repo "ag91/emacs-with-nyxt"
            :files ("emacs-with-nyxt.el")))
(package! doom-snippets :ignore t) ;; ignore doom's snippets
(package! org-wild-notifier)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; Own code:
(package! kindle-highlights-to-org
  :recipe '(:host github :repo "Zweihander-Main/kindle-highlights-to-org"))
(package! zweigtd-agenda
  :recipe '(:host github :repo "Zweihander-Main/zweigtd-agenda"
            :files ("*.el")))
(package! zweigtd-goals
  :recipe '(:host github :repo "Zweihander-Main/zweigtd-goals"))
(package! zweigtd-reviews
  :recipe '(:host github :repo "Zweihander-Main/zweigtd-reviews"
            :files ("zweigtd-reviews.el" "templates")))

;; Fix broken packages
;; (straight-use-package '(flymake :type built-in))
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; packages.el ends here
