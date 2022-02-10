;;; 74_ref --- ref/bib config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; `org-ref' and bibtex configuration.
;;;
;;; Code:

(map! (:map bibtex-mode-map
       :localleader
       ;; Nav
       :desc "Next entry" "j" #'org-ref-bibtex-next-entry
       :desc "Prev entry" "k" #'org-ref-bibtex-previous-entry
       ;; Open
       :desc "Open browser" "b" #'org-ref-open-in-browser
       :desc "Open notes" "n" #'org-ref-open-bibtex-notes
       :desc "Open PDF" "p" #'org-ref-open-bibtex-pdf
       ;; Attach
       :desc "Attach pdf" "a" #'org-ref-bibtex-assoc-pdf-with-entry
       ;; Insert
       :desc "Insert new entry" "i" #'org-ref-bibtex-hydra/org-ref-bibtex-file/body-and-exit
       :desc "Insert citation" "c" #'org-ref-insert-link
       :desc "Book ISBN -> entry" "b" #'isbn-to-bibtex
       :desc "URL -> entry" "u" #'org-ref-url-html-to-bibtex
       ;; Misc
       :desc "Actions on entry" "h" #'org-ref-bibtex-hydra/body
       :desc "Clean entry" "C" #'org-ref-clean-bibtex-entry
       :desc "Sort entry" "s" #'org-ref-sort-bibtex-entry
       :desc "Sort buffer" "S" #'bibtex-sort-buffer
       (:prefix ("l" . "lookup")
        :desc "ISBN: lead.to" "l" #'isbn-to-bibtex-lead
        :desc "ISBN: google" "g" #'zwei/ref-isbn-from-title))
      (:map org-mode-map
       :localleader
       (:prefix ("m" . "roam")
        :desc "Insert citation" "c" #'org-cite-insert))
      (:after markdown
       :map markdown-mode-map
       :localleader
       :desc "Insert citation" "c" #'org-ref-insert-link))

(eval-when-compile
  (defvar url-http-end-of-headers))

;;;###autoload
(defun zwei/ref-isbn-from-title (title)
  "Get ISBN-13 of TITLE using Google ISBN lookup.
No API key needed for minor use."
  (interactive (list (read-string "Enter title/keywords: ")))
  (let ((url-request-extra-headers'(("Accept" . "application/json")))
        json)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://books.googleapis.com/books/v1/volumes?q="
                 (url-hexify-string title)
                 "&printType=BOOKS"))
      (goto-char url-http-end-of-headers)
      (setq json (json-read))
      (if (not json)
          (message "Bad request")
        (let* ((items (cdr (assoc 'items json)))
               (collection
                (mapcar
                 (lambda (item)
                   (let* ((volInfo (assoc 'volumeInfo item))
                          (title (cdr (assoc 'title volInfo)))
                          (author (or (and
                                       (> (length
                                           (cdr (assoc 'authors volInfo)))
                                          0)
                                       (aref (cdr (assoc 'authors volInfo)) 0))
                                      "No Author"))
                          (ident (cdr (assoc 'industryIdentifiers volInfo)))
                          (isbn
                           (pcase (length ident)
                             (0 "NO ISBN, DO NOT SELECT")
                             (1 (cdr (assoc 'identifier (aref ident 0))))
                             (_ (cdr (assoc 'identifier (aref ident 1)))))))
                     (cons (concat title " by " author (format " (%s)" isbn))
                           isbn)))
                 items)))
          (cdr (assoc (completing-read
                       "Select which book to use: "
                       collection
                       nil
                       t)
                      collection)))))))

(use-package! org-ref
  :commands (org-ref-bibtex-next-entry
             org-ref-bibtex-previous-entry
             org-ref-open-in-browser
             zwei/bibtex-open-roam-at-point
             org-ref-open-bibtex-notes
             org-ref-open-bibtex-pdf
             org-ref-bibtex-assoc-pdf-with-entry
             org-ref-bibtex-hydra/org-ref-bibtex-file/body-and-exit
             org-ref-insert-link
             isbn-to-bibtex
             org-ref-url-html-to-bibtex
             org-ref-bibtex-hydra/body
             org-ref-clean-bibtex-entry
             org-ref-sort-bibtex-entry
             bibtex-sort-buffer
             isbn-to-bibtex-lead
             zwei/ref-isbn-from-title)
  :after org-roam
  :hook bibtex-mode-hook
  :config
  (require 'citar-org)
  (let ((bib-files
         (mapcar (lambda (f)
                   (expand-file-name f zwei/org-roam-bib-directory))
                 zwei/org-roam-bib-files)))
    (setq bibtex-completion-bibliography bib-files
          org-cite-global-bibliography bib-files
          bibtex-completion-notes-path zwei/org-roam-bib-directory
          bibtex-completion-library-path `(,zwei/org-roam-bib-files-directory)
          org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar
          ;; Rules for automatic key gen
          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 5
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5))
  (require 'org-ref-url-utils)
  (require 'org-ref-isbn))

;;; 74_ref ends here
