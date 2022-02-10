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
      auto-save-interval 3)

;;; 10_backup ends here
