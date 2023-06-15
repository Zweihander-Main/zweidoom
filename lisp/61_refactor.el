;;; 61_refactor.el --- settings for elisp refactoring -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Settings for elisp refactoring. Currently using erefactor.
;;;
;;; Code:

(map! (:localleader
       :map (emacs-lisp-mode-map lisp-interaction-mode-map)
       :prefix ("r" . "refactor")
       :desc "Rename symbol in package" "R" #'erefactor-rename-symbol-in-package
       :desc "Add current defun" "A" #'erefactor-add-current-defun
       :desc "Change prefix in buffer" "c" #'erefactor-change-prefix-in-buffer
       :desc "Dehighlight all symbol" "d" #'erefactor-dehighlight-all-symbol
       :desc "Highlight current symbol" "h" #'erefactor-highlight-current-symbol
       :desc "Lint by emacsen" "l" #'erefactor-lint-by-emacsen
       :desc "Rename symbol" "r" #'erefactor-rename-symbol-in-buffer
       :desc "Eval current defun" "x" #'erefactor-eval-current-defun
       :desc "Flymake display errors" "?" #'erefactor-flymake-display-errors))

(use-package! erefactor
  :hook (emacs-lis-mode-hook))

;;; 61_refactor.el ends here
