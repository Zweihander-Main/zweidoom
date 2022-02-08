;;; config.el -- doom/config.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Loading files after package init mostly to reduce a layer of paran in code.
;;;
;;; Code:

;; Load Lisp dir
(use-package! init-loader
  :config
  (setq init-loader-directory (file-name-as-directory
                               (expand-file-name "lisp" doom-private-dir))
        init-loader-show-log-after-init 'error-only
        init-loader-byte-compile nil)
  (init-loader-load))

;; General
(load! "+app")

;; Org
(after! org
  (after! org-capture
    (load! "./org/+capture"))
  (after! org-agenda
    (load! "./org/+agenda")
    (load! "./org/+custom-commands"))
  (after! org-roam
    (load! "./org/+roam")))

;; Other apps
(after! mu4e
  (load! "./+mu4e"))

;;; config.el ends here
