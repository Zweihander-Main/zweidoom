;;; 71_clock --- basic org-clock config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Basic org-clock configuration.
;;;
;;; Code:

(eval-when-compile
  (declare-function org-todo "org"))

(add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)

;;;###autoload
(defun zwei/set-todo-state-next ()
  "Change todo to NEXT."
  (org-todo "NEXT"))

(use-package! org-clock
  :commands (zwei/set-todo-state-next))

;;; 71_clock ends here
