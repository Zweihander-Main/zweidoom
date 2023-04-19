;;; 73_citar --- citar configuration -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Citar configuration.
;;;
;;; Code:

(eval-when-compile
  (defvar embark-keymap-alist))

(use-package! citar
  :when (modulep! :completion vertico)
  :after embark
  :commands (citar-insert-citation)
  :defer t
  :config
  (add-to-list 'embark-keymap-alist '(bibtex . citar-map)))

;;; 73_citar ends here
