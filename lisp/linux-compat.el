;;; linux-compat --- linux only config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;; Linux only commands to maintain cross OS compat.
;;;
;;; Code:

(let ((distro (zwei/which-linux-distro)))
  ;; Load lisp files for compiled Emacs on Debian/WSL/Arch
  (when (or (string= distro "Debian")
            (string= distro "Arch"))
    (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

  ;; System trash
  (when (string= distro "Arch")
    (setq delete-by-moving-to-trash t)
    (defun system-move-file-to-trash (file)
      "Send FILE to trash using `trash-put'."
      (call-process "trash-put" nil nil nil file)))

  ;; WSL only
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe"))
    (when (file-exists-p cmd-exe)
      ;; Enable emacs to open links in Windows
      (setq browse-url-generic-program cmd-exe
            browse-url-generic-args '("/c" "start")
            browse-url-browser-function 'browse-url-generic)

      ;; Copy in WSL
      (defun wsl-copy (start end)
        (interactive "r")
        (shell-command-on-region start end "clip.exe")
        (deactivate-mark))

      ;; Paste in WSL
      (defun wsl-paste ()
        (interactive)
        (let ((clipboard
               (shell-command-to-string
                "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
          (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
          (setq clipboard (substring clipboard 0 -1))
          (insert clipboard))))))


;;; linux-compat ends here
