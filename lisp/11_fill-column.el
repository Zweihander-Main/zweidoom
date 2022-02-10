;;; 11_fill-column --- fill-column config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Modify the fill-column (line to the right of the buffer).
;;;
;;; Code:

(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'display-fill-column-indicator-mode)

;;; 11_fill-column ends here
