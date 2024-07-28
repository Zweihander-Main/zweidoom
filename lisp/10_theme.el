;;; 10_theme --- theming -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; If it themes, it's here.
;;;
;;; Code:

(defun zwei/font-exists-p (font-name)
  "Check if a specific FONT-NAME is available on the user's system."
  (when(display-graphic-p)
    (let ((font (x-list-fonts font-name)))
      (if font t nil))))

(setq doom-theme 'doom-tomorrow-night
      doom-font (if (zwei/font-exists-p "Iosevka SS09 Extended")
                    (font-spec :family "Iosevka SS09 Extended" :size 11.0))
      doom-symbol-font (if (zwei/font-exists-p "Iosevka Term SS09 Extended")
                           (font-spec :family "Iosevka Term SS09 Extended" :size 11.0))
      doom-variable-pitch-font (if (zwei/font-exists-p "Iosevka Aile")
                                   (font-spec :family "Iosevka Aile" :size 12.0))
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
  '(line-number-current-line :inherit line-number))

(if (zwei/font-exists-p "Iosevka Term SS09")
    (custom-set-faces!
      '(line-number :family "Iosevka Term SS09")))

;;; 10_theme ends here
