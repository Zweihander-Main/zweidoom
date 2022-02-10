;;; 11_ui-org --- org ui config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Org related UI configuration.
;;;
;;; Code:

(after! org
  (setq org-ellipsis "▼")

  (doom-themes-org-config)

  (use-package! org-variable-pitch
    :defer t
    :after org
    :config
    (set-face-attribute 'org-variable-pitch-fixed-face nil
                        :family "Iosevka SS09 Extended")
    (add-hook 'org-mode-hook 'org-variable-pitch--enable))

  (add-hook! 'org-agenda-mode-hook #'solaire-mode #'hl-line-mode)

  ;; Link related improvements
  (after! ol ; org-link
    (eval-when-compile
      (declare-function org-link-set-parameters "ol"))
    (dolist (type '("ftp" "http" "https" "mailto" "news"))
      (org-link-set-parameters type :face `(:inherit org-link :slant italic)))
    (dolist (type '("elisp" "help" "file" "file+sys" "file+emacs" "org" "img" "doi" "bibtex" "attachment" "pdf"))
      (org-link-set-parameters type :face `(:inherit org-link :foreground "#DE935F")))
    (dolist (type '("roam" "id"))
      (org-link-set-parameters type :face `(:inherit org-link :foreground "#B5BD68"))))

  (custom-set-faces!
    `(org-habit-ready-face :background "#017105" )
    `(org-habit-ready-future-face :background "#015505")
    `(org-habit-alert-face :background "#A09000" )
    `(org-habit-alert-future-face :background "#807000")))

;;; 11_ui-org ends here
