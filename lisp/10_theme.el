;;; 10_theme --- theming -*-lexical-binding:t-*-
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
      display-line-numbers-type t
      line-spacing 0.1
      garbage-collection-messages nil
      split-height-threshold nil ;; Prefer vertical split
      split-width-threshold 0
      window-min-width 2
      window-min-height 1)

(add-hook! 'org-agenda-mode-hook #'solaire-mode #'hl-line-mode)

(custom-set-faces!
  '(highlight :background "#DE935F")
  '(line-number :family "Iosevka Term SS09")
  '(line-number-current-line :inherit line-number))

;;; 10_theme ends here