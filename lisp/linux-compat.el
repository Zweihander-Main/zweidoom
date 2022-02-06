;;; linux-compat --- linux only config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;; Linux only commands to maintain cross OS compat.
;;;
;;; Code:

(eval-when-compile
  (declare-function zwei/which-linux-distro ""))

;; System trash
(when (string= (zwei/which-linux-distro) "Arch")
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Send FILE to trash using `trash-put'."
    (call-process "trash-put" nil nil nil file)))

;;; linux-compat ends here
