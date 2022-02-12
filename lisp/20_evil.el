;;; 20_evil --- evil related config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Something something darkside.
;;;
;;; Code:

(use-package! evil-motion-trainer
  :after evil
  :defer t
  :config
  (global-evil-motion-trainer-mode 1))

;;; 20_evil ends here
