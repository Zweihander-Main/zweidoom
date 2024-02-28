;;; 02_util --- useful util funcs for Emacs -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Common util functions for Emacs used throughout the config.
;;;
;;; Code:

(defun zwei/which-linux-distro ()
  "Info from lsb_release. Will output strings such as 'Debian' or 'Arch'."
  (when (eq system-type 'gnu/linux)
    (let ((output (process-lines "lsb_release" "-is")))
      (and output (car output)))))

(defun zwei/port-in-use-by-emacs (port)
  "Will return t if PORT has been created by Emacs, nil otherwise."
  (when (and (eq system-type 'gnu/linux)
             (not (string-empty-p
                   (shell-command-to-string
                    (format "lsof -i :%d | grep emacs" port)))))
    t))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;;; 02_util.el ends here
