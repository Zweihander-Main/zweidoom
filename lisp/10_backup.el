;;; 10_backup --- backup/save -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Backup and save related configuration.
;;;
;;; Code:

(setq delete-old-versions -1
      backup-by-copying t ; don't clobber symlinks
      kept-new-versions 10 ; keep 10 versions
      delete-old-versions t ; delete old versions silently
      version-control t ; number backups
      vc-make-backup-files t ; backup version controlled files
      auto-save-interval 20 ; Auto-save every 20 characters typed
      auto-save-visited-interval 5) ; Auto-save after 5 seconds of idle time

;; Automatic saving direct to files
(auto-save-visited-mode +1)

;; Save all buffers on focus lost
(add-hook! '(doom-switch-buffer-hook
             doom-switch-window-hook)
  (if (buffer-file-name) (save-some-buffers t))) ; avoid saving when switching to a non-file buffer
(add-function :after after-focus-change-function
              (lambda () (save-some-buffers t)))

;;; 10_backup ends here
