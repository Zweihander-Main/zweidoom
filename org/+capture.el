;;; +capture.el -- doom/org/+capture.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to org capture and reviews called from it.
;;;
;;; Code:

(require 'org)
(require 'org-capture)

;; Hacks
; From https://github.com/hlissner/doom-emacs/issues/4832#issuecomment-822845907
; Stops weird C-c override bugs
(defalias '+org--restart-mode-h #'ignore)

;; Functions

(defun zwei/org-inbox-capture ()
  "Shortcut to org-capture->inbox."
  (interactive)
  "Capture a an inbox task."
  (org-capture nil "i"))

;; Config
(setq org-capture-templates
      `(("i" "inbox"
         entry
         (file ,zwei/org-agenda-todo-file)
         "* TODO %?")
        ("n" "next"
         entry
         (file ,zwei/org-agenda-next-file)
         "* NEXT [#%^{Priority?|C|A|B|C}] %? %^g:@work: %^{Effort}p ")
        ("c" "org-protocol-capture"
         entry
         (file ,zwei/org-agenda-todo-file)
         "* TODO [[%:link][%:description]]\n\n %i"
         :immediate-finish t)))

(use-package! zweigtd-reviews
  :config
  (setq zweigtd-reviews-file zwei/org-agenda-reviews-file)
  (zweigtd-reviews-default-bootstrap))

;; Override doom popup rules for org-capture, allow fullscreen
(set-popup-rules!
  '(("^\\*Capture\\*$\\|CAPTURE-.*$"
     :size 1.00 ; Allow full screen for reviews
     :quit nil
     :select t
     :autosave ignore)
    ("^\\*anki\\*$"
     :size 0.5
     :select t
     :autosave ignore)))

;;; +capture.el ends here
