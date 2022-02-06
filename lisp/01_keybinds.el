;;; 01_keybinds --- global keybindings -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;; Global shortcuts/keybindings.
;;;
;;; Code:

(eval-when-compile
  (defvar org)
  (defvar org-agenda)
  (defvar org-capture)
  (defvar org-roam-bibtex)
  (defvar org-roam-directory)
  (defvar +roam2)
  (defvar zwei/org-agenda-directory)
  (defvar zwei/find-gtd-file)
  (defvar zwei/deft-gtd-file)
  (defvar zwei/org-agenda-force-load)
  (defvar zwei/org-inbox-capture)
  (defvar zwei/bib+ref+roam-book-title)
  (declare-function map! "")
  (declare-function featurep! "")
  (declare-function org-roam-buffer--visibility "")
  (declare-function org-roam-buffer-toggle "")
  (declare-function evil-goto-first-line "")
  (declare-function cmd! "")
  (declare-function use-package! "")
  (declare-function zwei/find-gtd-file "")
  (declare-function zwei/deft-in-dir "")
  (declare-function zwei/org-inbox-capture "")
  (declare-function zwei/bib+ref+roam-book-title ""))

(defun zwei/org-agenda-force-load (key)
  "Go to agenda KEY and stick to the first line.
Used for global agenda-access keys."
  (when (and (featurep! :lang org +roam2)
             (functionp #'org-roam-buffer--visibility)
             (eq 'visible (org-roam-buffer--visibility)))
    (org-roam-buffer-toggle))
  (org-agenda nil key)
  (evil-goto-first-line))

(map! :g "<f1>" (cmd! (zwei/org-agenda-force-load "1"))
      :g "<f2>" (cmd! (zwei/org-agenda-force-load "2"))
      :g "<f3>" (cmd! (zwei/org-agenda-force-load "3")))


;; Experiment: add command hooks alongside globals
(use-package! org
  :commands zwei/find-gtd-file zwei/deft-gtd-file)
(use-package! org-agenda
  :commands zwei/org-agenda-force-load)
(use-package! org-capture
  :commands zwei/org-inbox-capture)
(use-package! org-roam-bibtex
  :commands zwei/bib+ref+roam-book-title)

(map! :leader
      :prefix "n"
      :when (featurep! :lang org)
      :desc "Find in gtd" :g "g" #'zwei/find-gtd-file
      :desc "Deft in gtd" :g "G" (cmd! (zwei/deft-in-dir zwei/org-agenda-directory))
      :desc "Inbox entry" :g "i" #'zwei/org-inbox-capture
      :when (featurep! :lang +roam2)
      :desc "Deft in roam" :g "d" (cmd! (zwei/deft-in-dir org-roam-directory))
      :prefix "nr"
      :desc "Create book bib+roam" :g "C" #'zwei/bib+ref+roam-book-title)

;;; 01_keybinds ends here
