;;; 00_debug --- tools for debugging emacs -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Tools for debugging Emacs.
;;;
;;; Code:

(defmacro zwei/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro zwei/measure-n-invocations (times &rest body)
  "Measure the average time it takes to evaluate BODY n TIMES."
  `(let ((results '()))
     (dotimes (i ,times)
       (let ((time (current-time)))
         ,@body
         (push (float-time (time-since time)) results)))
     (message "%.06f" (/ (apply '+ results) (length results)))))

;; https://github.com/hlissner/doom-emacs/issues/4498
(when init-file-debug
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

;;; 00_debug ends here
