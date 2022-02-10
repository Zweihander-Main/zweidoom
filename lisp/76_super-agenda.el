;;; 76_super-agenda --- super-agenda config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-super-agenda' configuration.
;;;
;;; Code:

(use-package! org-super-agenda
  :after evil-org-agenda
  :config
  (eval-when-compile
    (defvar evil-org-agenda-mode-map)
    (declare-function org-super-agenda-mode ""))
  (org-super-agenda-mode t)
  (setq org-super-agenda-header-map evil-org-agenda-mode-map)) ; fix keymaps

;;; 76_super-agenda ends here
