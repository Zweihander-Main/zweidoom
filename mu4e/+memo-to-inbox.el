;;; +memo-to-inbox.el -- doom/mu4e/+memo-to-inbox.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Mu4e function to get the memo folder and add it to the inbox for the
;;; agenda to later process.
;;;
;;; Code:

(require 'mu4e)
(require 'org-mu4e)
(require 'org-capture)

;; Variables

(defvar zwei/mu4e-memo-to-inbox-state "mu4e-memo-to-inbox"
  "Name of state to save for memo to inbox save/restore functionality.")

;; Functions

(defun zwei/mu4e-script-mode (&optional disable)
  "Will enable script mode unless DISABLE is non-nil."
  (if disable
      (progn
        (setq mu4e-split-view 'vertical)
        (zwei/save-and-restore-state "mu4e-memo-to-inbox" "clear"))
    (progn
      (setq mu4e-split-view nil))))

(defun zwei/mu4e-memo-to-inbox ()
  "Pull in emails to memo folder and convert them to org headings in the inbox.
Use the email body for content and mark the emails as read. This is the first
part of the function which calls the search and saves the location to restore
 after the search is completed."
  (interactive)
  (mu4e-context-switch nil "fastmail")
  (zwei/save-and-restore-state zwei/mu4e-memo-to-inbox-state "write")
  (zwei/mu4e-script-mode)
  (mu4e-headers-search-bookmark "flag:unread AND NOT flag:trashed AND maildir:/fastmail/memo"))

(defun zwei/mu4e-buffer-manage (buffer &optional switch)
  "BUFFER can be 'view' or 'headers'.
Returns true if current buffer is specified buffer.
Will switch to that buffer is SWITCH is non-nil."
  (let* ((target-buffer (cond ((string= buffer "view") (mu4e-get-view-buffer))
                              ((string= buffer "headers") (mu4e-get-headers-buffer))
                              (t (progn (user-error "BUFFER should be 'view' or 'headers'")
                                        (current-buffer)))))
         (is-buffer (eq (current-buffer) target-buffer)))
    (when (and target-buffer (not is-buffer) switch)
      (switch-to-buffer target-buffer))
    (setq is-buffer (eq (current-buffer) target-buffer))))

(defun zwei/mu4e-memo-to-inbox-view-message-and-process ()
  "Process the message, add to the org inbox file, and restore the original
state if it's the last message."
  (when (and ;; Only execute if in view and we're processing the inbox
         (zwei/save-and-restore-state zwei/mu4e-memo-to-inbox-state "read")
         (zwei/mu4e-buffer-manage "view"))
    (princ (mu4e-message-at-point t))
    (let ((msg (mu4e-message-at-point t)))
      (when msg
        (let* ((body (string-trim (mu4e-body-text msg)))
               (subject (string-trim (mu4e-message-field msg :subject)))
               (body-to-set (cond
                             ((string= "" body) nil)
                             ((string= subject body) nil)
                             ;; ((string= "" subject) (mapconcat 'identity (cdr (split-string body "\\n" nil " ")) "\n"))
                             (t (string-trim body))))
               (header-to-set (cond
                               ((string= "" subject) (car (split-string body "\\n" nil " ")))
                               (t (string-trim subject)))))
          (org-capture nil "i")
          (funcall-interactively 'org-edit-headline header-to-set)
          (when body-to-set
            (insert "\n")
            (insert body-to-set))
          (org-capture-finalize)
          (zwei/mu4e-buffer-manage "headers" t)
          (mu4e-mark-set 'read)
          (let ((next (mu4e-headers-next)))
            (if next
                (mu4e-headers-view-message)
              (progn
                (when (buffer-live-p (mu4e-get-view-buffer))
                  (kill-buffer (mu4e-get-view-buffer)))
                (mu4e-mark-execute-all t) ;; Execute all marks, there should be at least one from this call
                (zwei/save-and-restore-state zwei/mu4e-memo-to-inbox-state "restore") ;; Done with memo-to-inbox
                (zwei/mu4e-script-mode t)))))))))

(defun zwei/mu4e-memo-to-inbox-process-found-headers ()
  "Hooked to call after a search for memos is completed.
Will setup the mu4e-view which is then hooked into the process function."
  (when (and ;; Only execute if in headers view and the previous function was called
         (zwei/save-and-restore-state zwei/mu4e-memo-to-inbox-state "read")
         (zwei/mu4e-buffer-manage "headers" t))
    (when (buffer-live-p (mu4e-get-view-buffer))
      (kill-buffer (mu4e-get-view-buffer)))
    (if (mu4e-message-at-point t)
        (progn (append-to-file "\n" nil zwei/org-agenda-todo-file)
               (mu4e-headers-view-message))
      (progn (zwei/save-and-restore-state zwei/mu4e-memo-to-inbox-state "restore") ;; no messages, reset it right out the gate
             (zwei/mu4e-script-mode t)))))

;; Hooks

(add-hook 'mu4e-view-mode-hook 'zwei/mu4e-memo-to-inbox-view-message-and-process)
(add-hook 'mu4e-headers-found-hook 'zwei/mu4e-memo-to-inbox-process-found-headers)
(add-hook 'mu4e-index-updated-hook 'zwei/mu4e-memo-to-inbox)

;; Init

(zwei/mu4e-script-mode t)

;;; +memo-to-inbox.el ends here
