;;; linux-compat --- linux only config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;; Linux only commands to maintain cross OS compat.
;;;
;;; Code:

(eval-when-compile
  (declare-function zwei/which-linux-distro ""))

;; Load lisp files for compiled Emacs on Debian/WSL/Arch
(when (or (string= (zwei/which-linux-distro) "Debian")
          (string= (zwei/which-linux-distro) "Arch"))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

;; System trash
(when (string= (zwei/which-linux-distro) "Arch")
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Send FILE to trash using `trash-put'."
    (call-process "trash-put" nil nil nil file)))

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


;;; linux-compat ends here
