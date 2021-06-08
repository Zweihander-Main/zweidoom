;; packages.el --- Located at $DOOMDIR/packages.el-*-lexical-binding:t-*-
;;; Commentary:

;;; Code:
(package! org-roam-server)
(package! org-ql)
(package! anki-editor)
(package! org-clock-convenience)
(package! vimrc-mode)
(package! flycheck-elsa)
(package! flycheck-package)
(package! org-variable-pitch
  :recipe '(:host github
            :repo "cadadr/elisp"
            :files ("org-variable-pitch.el")))
(package! evil-motion-trainer
  :recipe '(:host github :repo "martinbaillie/evil-motion-trainer"))

;; Own code:
(package! kindle-highlights-to-org
  :recipe '(:host github :repo "Zweihander-Main/kindle-highlights-to-org"))
(package! process-org-agenda-inbox
  :recipe '(:host github :repo "Zweihander-Main/process-org-agenda-inbox"))
(package! org-statistics-cookie-helpers
  :recipe '(:host github :repo "Zweihander-Main/org-statistics-cookie-helpers"))
(package! org-agenda-heading-functions
  :recipe '(:host github :repo "Zweihander-Main/org-agenda-heading-functions"))
(package! zweigtd-goals
  :recipe '(:host github :repo "Zweihander-Main/zweigtd-goals"))
(package! zweigtd-reviews
  :recipe '(:host github :repo "Zweihander-Main/zweigtd-reviews"
            :files ("zweigtd-reviews.el" "templates")))
(package! mu4e-search-to-org
  :recipe '(:host github :repo "Zweihander-Main/mu4e-search-to-org"))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; packages.el ends here
