;;; 81_roam-bibtex --- roam/org-ref integration -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-ref' and `org-roam' integration config.
;;;
;;; Code:

(map! (:leader
       (:prefix "n"
        (:prefix "r"
         :desc "Create book bib+roam" :g "C" #'zwei/bib+ref+roam-book-title)))
      (:map bibtex-mode-map
       :localleader
       :desc "Open roam entry" "r" #'zwei/bibtex-open-roam-at-point)
      (:map org-mode-map
       :localleader
       (:prefix ("m" . "roam")
        :desc "Open citation roam entry" "RET" #'zwei/org-roam-open-citation-roam-entry)))

(eval-when-compile
  (declare-function bibtex-completion-key-at-point "bibtex-completion")
  (declare-function org-ref-get-bibtex-key-under-cursor "org-ref-citation-links"))

;;;###autoload
(defun zwei/bibtex-open-roam-at-point ()
  "Open roam entry using orb for current point."
  (interactive)
  (let ((citekey (bibtex-completion-key-at-point)))
    (if (not citekey)
        (message "Citekey not found, bib entry likely not created.")
      (orb-edit-note citekey))))

;;;###autoload
(defun zwei/org-roam-open-citation-roam-entry ()
  "Open roam entry related to citation under cursor."
  (interactive)
  (let ((citekey (org-ref-get-bibtex-key-under-cursor)))
    (if (not citekey)
        (message "Citekey not found.")
      (orb-edit-note citekey))))

;;;###autoload
(defun zwei/bib+ref+roam-book-title (title)
  "Prompt user for TITLE, ask API for ISBN, create bibtex entry + roam note."
  (interactive (list (read-string "Enter title/keywords: ")))
  (let ((isbn (zwei/ref-isbn-from-title title))
        (book-bib (expand-file-name (car zwei/org-roam-bib-files)
                                    zwei/org-roam-bib-directory)))
    (isbn-to-bibtex isbn book-bib)
    (if (not (string= (buffer-file-name) book-bib))
        (message "isbn-to-bibtex wasn't able to find data for that ISBN.")
      (progn
        (zwei/bibtex-open-roam-at-point)
        (set-buffer-modified-p t)
        (save-buffer)))))

(use-package! org-roam-bibtex
  :after org-roam
  :commands (zwei/bibtex-open-roam-at-point
             zwei/bib+ref+roam-book-title
             zwei/org-roam-open-citation-roam-entry)
  :config
  (setq orb-preformat-keywords
        '("citekey"
          "entry-type"
          "date"
          "pdf?"
          "note?"
          "file"
          "author"
          "editor"
          "author-or-editor"
          "author-abbrev"
          "editor-abbrev"
          "author-or-editor-abbrev"
          "title"
          "keywords"
          "url"))

  ;; ORB capture templates
  (add-to-list 'org-roam-capture-templates
               `("o" "orb: book-capture"
                 plain
                 "%?"
                 :target (file+head
                          ,(concat "bib/" "%<%Y%m%d%H%M%S>-${citekey}" ".org")
                          ,(concat "#+TITLE: ${title} by ${author-abbrev}\n"
                                   "\n"
                                   "- related :: \n"
                                   "* ${title}\n"
                                   ":PROPERTIES:\n"
                                   ":CUSTOM_ID: ${citekey}\n"
                                   ":AUTHOR: ${author}\n"
                                   ":KEYWORDS: ${keywords}\n"
                                   ":END:\n"
                                   "* Notes:\n"
                                   "- "))
                 :immediate-finish t
                 :unnarrowed t)))

;;; 81_roam-bibtex ends here
