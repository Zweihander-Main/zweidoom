;;; 05_message --- message buffer -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Modifications to message buffer behaviour.
;;;
;;; Code:

(defadvice message (after message-tail activate)
  "Goto point max after a new message."
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (while windows
        (set-window-point (car windows) (point-max))
        (setq windows (cdr windows))))))

;;; 05_message ends here
