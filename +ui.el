;;; +ui.el -- doom/+ui.el-*-lexical-binding:t-*-
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
(doom-init-extra-fonts-h)

(use-package! org-variable-pitch
  :defer t
  :after org
  :config
  (set-face-attribute 'org-variable-pitch-fixed-face nil
                      :family "Iosevka SS09 Extended")
  (add-hook 'org-mode-hook 'org-variable-pitch--enable))


(add-hook! 'org-agenda-mode-hook #'(solaire-mode hl-line-mode))

(custom-set-faces!
  '(highlight :background "DarkOrange3")
  '(org-roam-link :inherit org-link
                  :foreground "DarkOrange3")
  '(line-number :family "Iosevka Term SS09")
  '(line-number-current-line :inherit line-number)
  `(org-habit-ready-face :background "#017105" )
  `(org-habit-ready-future-face :background "#015505")
  `(org-habit-alert-face :background "#A09000" )
  `(org-habit-alert-future-face :background "#807000"))

;;; +ui.el ends here
