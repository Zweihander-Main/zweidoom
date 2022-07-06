;;; 03_signals.el --- bindings for user signals -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Bindings for SIGUSR1/2
;;;
;;; Code:

(defun zwei/sigusr1-handler()
  "Handle SIGUSR1 signal by saving all buffers."
  (interactive)
  (evil-write-all nil))

(define-key special-event-map [sigusr1] 'zwei/sigusr1-handler)

;;; 03_signals.el ends here
