;;; 50_deft --- deft config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `deft'configuration.
;;;
;;; TODO: move roam config into roam file
;;;
;;; Code:

(map! :leader
      (:prefix "n"
       :desc "Deft in gtd" :g "G"
       (cmd! (zwei/deft-in-dir zwei/org-agenda-directory))))

;;;###autoload
(defun zwei/deft-in-dir (dir)
  "Run deft in directory DIR."
  (require 'org)
  (setq deft-directory dir)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (deft))

(use-package! deft
  :commands (deft zwei/deft-in-dir)
  :config
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

;;; 50_deft ends here
