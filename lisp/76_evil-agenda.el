;;; 76_evil-agenda --- agenda vi bindings-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config to make sure that agenda has full evil integration.
;;; Used to make sure that plugins like easymotion still work in agenda.
;;;
;;; Code:

(use-package! evil-org-agenda
  :when (modulep! :editor evil +everywhere)
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (require 'evil-easymotion)
  (map! :map evil-org-agenda-mode-map
        :m "gs" evilem-map))

;;; 76_evil-agenda ends here
