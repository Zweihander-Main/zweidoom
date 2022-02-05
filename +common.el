;;; +common.el -- doom/+common.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains common variables and functions used repeatedly throughout the
;;; config.
;;;
;;; Code:

;; ==================
;;  Common Functions
;; ==================

(defun zwei/which-linux-distro ()
  "Info from lsb_release. Will output strings such as 'Debian' or 'Arch'."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "echo -n $(lsb_release -is)")))

(defun zwei/port-in-use-by-emacs (port)
  "Will return t if PORT has been created by emacs, nil otherwise."
  (when (and (eq system-type 'gnu/linux)
             (not (string-empty-p
                   (shell-command-to-string
                    (format "lsof -i :%d | grep emacs" port)))))
    t))
;;; +common.el ends here
