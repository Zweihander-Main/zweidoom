;;; 02_util --- useful util funcs for Emacs -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Common util functions for Emacs used throughout the config.
;;;
;;; Code:

(defun zwei/which-linux-distro ()
  "Info from lsb_release. Will output strings such as 'Debian' or 'Arch'."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "echo -n $(lsb_release -is)")))

(defun zwei/port-in-use-by-emacs (port)
  "Will return t if PORT has been created by Emacs, nil otherwise."
  (when (and (eq system-type 'gnu/linux)
             (not (string-empty-p
                   (shell-command-to-string
                    (format "lsof -i :%d | grep emacs" port)))))
    t))

;;; 02_util ends here
