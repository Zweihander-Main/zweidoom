;;; 62_copilot.el --- settings for copilot -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Github Copilot settings.
;;;
;;; Code:

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-SPC" . 'copilot-accept-completion)))

;;; 62_copilot.el ends here
