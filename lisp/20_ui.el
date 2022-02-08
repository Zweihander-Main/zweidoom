;;; ui --- theming -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; If it themes, it's here.
;;;
;;; Code:

(setq doom-theme 'doom-tomorrow-night
      doom-font (font-spec :family "Iosevka SS09 Extended" :size 15)
      doom-unicode-font (font-spec :family "Iosevka Term SS09 Extended" :size 15)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      doom-serif-font doom-variable-pitch-font
      doom-themes-treemacs-theme "doom-colors"
      org-ellipsis "▼"
      display-line-numbers-type t
      line-spacing 0.1
      garbage-collection-messages nil
      split-height-threshold nil ;; Prefer vertical split
      split-width-threshold 0
      window-min-width 2
      window-min-height 1)

(doom-themes-org-config)

(use-package! org-variable-pitch
  :defer t
  :after org
  :config
  (set-face-attribute 'org-variable-pitch-fixed-face nil
                      :family "Iosevka SS09 Extended")
  (add-hook 'org-mode-hook 'org-variable-pitch--enable))


(add-hook! 'org-agenda-mode-hook #'solaire-mode #'hl-line-mode)

(custom-set-faces!
  '(highlight :background "#DE935F")
  '(line-number :family "Iosevka Term SS09")
  '(line-number-current-line :inherit line-number)
  `(org-habit-ready-face :background "#017105" )
  `(org-habit-ready-future-face :background "#015505")
  `(org-habit-alert-face :background "#A09000" )
  `(org-habit-alert-future-face :background "#807000"))

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

;;; ui ends here
