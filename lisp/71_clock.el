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
  :commands (zwei/set-todo-state-next
             org-agenda-clock-in
             org-agenda-clock-out
             org-agenda-clock-goto
             org-agenda-clock-cancel
             org-agenda-clockreport-mode
             org-clock-in
             org-clock-out
             org-clock-goto
             org-clock-cancel
             org-clock-sum
             org-clock-report
             org-clock-resolve
             org-clock-load
             org-clock-clock-in
             org-clock-clock-out))

;;; 71_clock ends here
