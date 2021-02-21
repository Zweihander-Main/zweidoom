;;; config.el -- ~/.doom.d/config.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Loading files after package init mostly to reduce a layer of paran in code.
;;;
;;;
;;; Code:

;; General
(load! "+common")
(load! "+app")
(load! "+ui")

;; Org
(after! org
  (load! "./org/+org")
  (load! "./org/+org-habit"))
(after! org-capture
  (load! "./org/+org-capture")
  (load! "./org/+reviews"))
(after! org-agenda
  (load! "./org/+org-agenda")
  (load! "./org/+org-agenda-process-inbox")
  (load! "./org/+org-agenda-custom-commands"))
(after! org-roam
  (load! "./org/+org-roam"))

;; Other apps
(after! mu4e
  (load! "./mu4e/+mu4e")
  (load! "./mu4e/+mu4e-memo-to-inbox"))

;; Code
(load! "+code")

;;; config.el ends here
