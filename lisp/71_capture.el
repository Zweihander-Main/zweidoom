;;; 71_capture --- org-capture config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Config related to org capture and reviews called from it.
;;;
;;; Hacks:
;;; From https://github.com/hlissner/doom-emacs/issues/4832#issuecomment-822845907
;;; Stops weird C-c override bugs
;;; (defalias '+org--restart-mode-h #'ignore)
;;;
;;; Code:

(map! :leader
      (:prefix "n"
       :desc "Inbox entry" :g "i" #'zwei/org-inbox-capture))

;;;###autoload
(defun zwei/org-inbox-capture ()
  "Shortcut to org-capture->inbox."
  (interactive)
  "Capture a an inbox task."
  (org-capture nil "i"))

(use-package! org-capture
  :after org
  :config
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
           :immediate-finish t))))

;;; 71_capture ends here
