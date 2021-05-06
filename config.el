;;; config.el -- doom/config.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Loading files after package init mostly to reduce a layer of paran in code.
;;;
;;;
;; Code:

;; General
(load! "+common")
(load! "+app")
(load! "+ui")

;; Org
(after! org
  (load! "./org/+org")
  (after! org-habit
    (load! "./org/+habit"))
  (after! org-capture
    (load! "./org/+reviews")
    (load! "./org/+capture"))
  (after! org-agenda
    (load! "./org/+agenda")
    (load! "./org/+process-inbox")
    (load! "./org/+custom-commands"))
  (after! org-roam
    (load! "./org/+roam")))

;; Other apps
(after! mu4e
  (load! "./mu4e/+mu4e")
  (load! "./mu4e/+memo-to-inbox"))

;; Code
(load! "+code")

;;; config.el ends here
