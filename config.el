;;; config --- main doom config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Init-loader to load all the other Emacs files.
;;;
;;; Code:

;; Load Lisp dir
(use-package! init-loader
  :config
  (setq init-loader-directory (file-name-as-directory
                               (expand-file-name "lisp" doom-user-dir))
        init-loader-show-log-after-init 'error-only
        init-loader-byte-compile nil)
  (init-loader-load))

;;; config ends here
