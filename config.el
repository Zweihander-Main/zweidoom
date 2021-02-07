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
  (load! "./org/+org"))
(after! org-agenda
  (load! "./org/+org-agenda"))
(after! org-habit-plus
  (load! "./org/+org-habit-plus"))
(after! org-roam
  (load! "./org/+org-roam"))

;; Other apps
(after! mu4e
  (load! "./mu4e/+mu4e"))

;; Code
(load! "+code")

;;; config.el ends here
