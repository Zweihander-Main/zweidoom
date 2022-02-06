;;; windows-compat --- win only config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;; Window only commands to maintain cross OS compat.
;;;
;;; Code:

;; WSL only
(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    ;; Enable emacs to open links in Windows
    (setq browse-url-generic-program cmd-exe
          browse-url-generic-args cmd-args
          browse-url-browser-function 'browse-url-generic))

  ;; Copy in WSL
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark))

  ;; Paste in WSL
  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
      (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
      (setq clipboard (substring clipboard 0 -1))
      (insert clipboard))))

;;; windows-compat ends here
