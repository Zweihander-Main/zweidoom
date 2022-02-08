;;; +app.el -- doom/+app.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;; Configuration for the general usage of Emacs.
;;;
;;; Code:

(setq user-full-name "Zweihänder"
      user-mail-address "zweidev@zweihander.me")

;; Fix issues with emacs 27 warnings
(setq byte-compile-warnings '(cl-functions))
(setq warning-suppress-log-types '((package reinitialization)))


;; ===============
;;   Backup/Save
;; ===============
(setq delete-old-versions -1
      backup-by-copying t ;; don't clobber symlinks
      kept-new-versions 10 ;;keep 10 versions
      delete-old-versions t ;; delete old versions silently
      version-control t ;; number backups
      vc-make-backup-files t ;;backup version controlled files
      auto-save-interval 3)


;; ================
;;  Message Buffer
;; ================

(defadvice message (after message-tail activate)
  "Goto point max after a new message."
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (while windows
        (set-window-point (car windows) (point-max))
        (setq windows (cdr windows))))))


;; ===========
;;  Debugging
;; ===========

(defmacro zwei/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro zwei/measure-n-invocations (times &rest body)
  "Measure the average time it takes to evaluate BODY n TIMES."
  `(let ((results '()))
     (dotimes (i ,times)
       (let ((time (current-time)))
         ,@body
         (push (float-time (time-since time)) results)))
     (message "%.06f" (/ (apply '+ results) (length results)))))


;; ==========
;;  Flycheck
;; =========
(after! flycheck
  (require 'org-lint)
  (defun flycheck-org-lint-start (checker callback)
    "Flycheck mode for org lint"
    (funcall
     callback 'finished
     (save-excursion
       (mapcar
        (lambda (err)
          (goto-char (car err))
          (flycheck-error-new-at
           (org-current-line) (1+ (current-column))
           'warning (cadr err) :checker checker))
        (org-lint-link-to-local-file (org-element-parse-buffer))))))

  (flycheck-define-generic-checker 'org-lint
    "Syntax checker for org-lint."
    :start 'flycheck-org-lint-start
    :modes '(org-mode))

  (add-to-list 'flycheck-checkers 'org-lint)
  (setq-default flycheck-disabled-checkers '(proselint)))

(defun zwei/add-checkdoc-back-in ()
  "Stop DOOM from removing checkdoc in config but keep the other mods."
  (setq flycheck-disabled-checkers
        (delete 'emacs-lisp-checkdoc flycheck-disabled-checkers)))
(advice-add '+emacs-lisp-reduce-flycheck-errors-in-emacs-config-h
            :after #'zwei/add-checkdoc-back-in)

(add-hook! 'emacs-lisp-mode-hook #'flycheck-elsa-setup)
(add-hook! 'emacs-lisp-mode-hook #'flycheck-package-setup)


;; ========
;;  ispell
;; ========
(after! ispell
  :config
  (setq ispell-dictionary "en")
  (setq ispell-program-name "aspell")
  (setq spell-fu-directory "~/.config/aspell")
  (setq ispell-personal-dictionary "~/.config/aspell/en.pws"))


;; ================
;;  Lookup/Docsets
;; ================
(setq +lookup-open-url-fn #'eww)


;; =====
;;  Eww
;; =====
(setq eww-search-prefix "https://lite.duckduckgo.com/lite/?q=")


;; ======
;;  Deft
;; ======
(defun zwei/deft-in-dir (dir)
  "Run deft in directory DIR."
  (setq deft-directory dir)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (deft))

(after! deft
  (setq deft-use-filter-string-for-filename t
        deft-recursive t
        deft-strip-summary-regexp
        (concat "\\("
                "^:.+:.*\n" ; any line with a :SOMETHING:
                "\\|^#\\+.*\n" ; anyline starting with a #+
                "\\|[\n\t]" ; blank
                "\\|- related ::.*$"; related tag
                "\\)"))
  ;; Roam V2 fix titles
  (advice-add 'deft-parse-title :override
              (lambda (file contents)
                (if deft-use-filename-as-title
                    (deft-base-filename file)
                  (let* ((case-fold-search 't)
                         (begin (string-match "title: " contents))
                         (end-of-begin (match-end 0))
                         (end (string-match "\n" contents begin)))
                    (if begin
                        (substring contents end-of-begin end)
                      (format "%s" file)))))))


;; ==============
;;  Centaur Tabs
;; ==============
(after! centaur-tabs
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)

       ;; Prevent org and doom related buffers
       (string-prefix-p "*org-roam" name)
       (string-prefix-p "*Messages" name)
       (string-prefix-p "*scratch" name)
       (string-prefix-p "*doom" name)
       (string-prefix-p "*tide-server" name)
       (string-prefix-p "*vls" name)
       (string-prefix-p "*Org Agenda" name)
       (string-prefix-p "*Apropos" name)
       (string-prefix-p "*anki*" name)
       ;; Stop org-roam string from showing up
       (string-match-p (concat "[0-9]\\{14\\}" ".*-.*\\.org") name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       ))))

;; ===========================
;;  Global evil modifications
;; ===========================

(use-package! evil-motion-trainer
  :after evil
  :config
  (global-evil-motion-trainer-mode 1))

;; =======================
;;  Fill column indicator
;; =======================
(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'display-fill-column-indicator-mode)

;;; +app.el ends here
