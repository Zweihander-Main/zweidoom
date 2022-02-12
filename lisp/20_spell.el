;;; 20_theme --- theming -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; If it themes, it's here.
;;;
;;; Code:

(after! ispell
  (setq ispell-dictionary "en")
  (setq ispell-program-name "aspell")
  (setq spell-fu-directory "~/.config/aspell")
  (setq ispell-personal-dictionary "~/.config/aspell/en.pws"))

;;; 20_spell ends here
