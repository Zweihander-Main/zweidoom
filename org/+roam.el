;;; +roam.el -- doom/+roam.el-*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains all org-roam related config (roam, anki, ect.).
;;; Anything that has to do with knowledge management should
;;; go here.
;;;
;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-roam-protocol)

;; Variables
(defconst zwei/slip-boxes
  '(("d" "[d]efault --permanent" "")
    ("b" "[b]ib -- literature" "bib/")
    ("p" "[p]osts" "posts/")
    ("l" "[l]ife" "life/")
    ("w" "[w]ork" "work/"))
  "Zettelkasten slip boxes in (key name dir) format.")

;; Functions
(defun zwei/roam-rename (new-name)
  "Move current file to NEW-NAME. `org-roam' takes care of adjusting all links."
  (interactive (list (let ((filename (buffer-file-name)))
                       (read-file-name
                        (format "Enter new name of file (%s): "
                                (file-name-nondirectory filename))
                        nil
                        (file-name-nondirectory filename)
                        nil
                        (file-name-nondirectory filename)))))
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting file!" (buffer-name)))
    (rename-file filename new-name)
    (set-visited-file-name new-name t)
    (revert-buffer t t t)
    ;; trigger save-buffer for org-roam to regenerate `org-roam-buffer'.
    (set-buffer-modified-p t)
    (save-buffer)))

(defun zwei/roam-delete ()
  "Trashes current file, `org-roam' takes care of adjusting all links.
Requires working system trash."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting file!" (buffer-name)))
    (delete-file filename t)
    ;; trigger save-buffer for org-roam to regenerate `org-roam-buffer'.
    (set-buffer-modified-p t)
    (kill-current-buffer)))

(defun zwei/roam-move-to-slip-box (slip-box)
  "Move file to specified SLIP-BOX."
  (interactive (list (completing-read "Move to slip-box: "
                                      (mapcar (lambda (x)
                                                (nth 2 x))
                                              zwei/slip-boxes))))
  (let* ((fullpath (buffer-file-name))
         (filename (file-name-nondirectory fullpath))
         (new-name (f-join org-roam-directory slip-box filename)))
    (zwei/roam-rename new-name)))

(defun zwei/bib+ref+roam-book-title (title)
  "Prompt user for title, ask API for ISBN, create bibtex entry + roam note."
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

;; Functions
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

(defun zwei/bibtex-open-roam-at-point ()
  "Open roam entry using orb for current point."
  (interactive)
  (let ((citekey (bibtex-completion-key-at-point)))
    (if (not citekey)
        (message "Citekey not found, bib entry likely not created.")
      (orb-edit-notes citekey))))

(defun zwei/org-roam-open-citation-roam-entry ()
  "Open roam entry related to citation under cursor."
  (interactive)
  (let ((citekey (org-ref-get-bibtex-key-under-cursor)))
    (if (not citekey)
        (message "Citekey not found.")
      (orb-edit-notes citekey))))

(defun zwei/bibtex-actions-insert-org-ref-citation ()
  "Allows org-ref style citations using vertico completion."
  (interactive)
  (let ((key (car (bibtex-actions-read :rebuild-cache))))
    (when key
      (insert "cite:" key))))

(defun filter-out-p (str)
  "Filter out <p> tags from STR when exporting Anki notes."
  (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                            "" str))

;; Config
(setq  org-roam-tag-sources '(prop all-directories)
       org-roam-index-file (concat org-roam-directory "/index.org")
       ;; Regular template for each slip box
       org-roam-capture-templates
       (mapcar (lambda (x)
                 (let ((key  (nth 0 x))
                       (name (nth 1 x))
                       (dir  (nth 2 x)))
                   `(,key ,name
                          plain
                          (function org-roam--capture-get-point)
                          "%?"
                          :file-name ,(concat dir "%<%Y%m%d%H%M%S>-${slug}")
                          :head
                          ,(concat "#+TITLE: ${title}\n"
                                   "#+ROAM_ALIAS: \n"
                                   "#+ROAM_TAGS: \n"
                                   "\n"
                                   "- related :: \n"
                                   "\n"
                                   "*  ")
                          :immediate-finish t
                          :unnarrowed t)))
               zwei/slip-boxes)
       org-roam-capture-ref-templates
       '(("r" "ref"
          plain
          (function org-roam-capture--get-point)
          "%?"
          :file-name "bib/$%<%Y%m%d%H%M%S>-${slug}"
          :head
          ,(concat "#+TITLE: ${title}\n"
                   "#+ROAM_ALIAS: \n"
                   "#+ROAM_KEY: ${ref}"
                   "#+ROAM_TAGS: \n"
                   "\n"
                   "- related :: \n"
                   "\n"
                   "* Notes\n"
                   "- ")
          :immediate-finish t
          :unnarrowed t))
       org-roam-graph-viewer (pcase (zwei/which-linux-distro)
                               ("Arch" "/usr/bin/chromium")
                               (_ nil)))

;; Mappings
(map! :map org-mode-map
      :localleader
      (:prefix ("m" . "roam")
       :desc "Delete file" "D" #'zwei/roam-delete
       :desc "Rename file" "R" #'zwei/roam-rename
       :desc "Move slipbox" "M" #'zwei/roam-move-to-slip-box))

;; (use-package! org-roam-server
;;   :after-call org-roam-server-mode
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 38080
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files t
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil  ; look into
;;         org-roam-server-cite-edge-dashes t
;;         org-roam-server-extra-cite-edge-options nil
;;         org-roam-server-style nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20
;;         org-roam-server-default-exclude-filters ;; TODO
;;         (json-encode (list (list (cons 'parent "bib"))))))

;; ;; Prevent doom error with smartparens and stop multiple instances
;; (after! org-roam
;;   (smartparens-global-mode -1)
;;   (unless (or org-roam-server-mode
;;               (zwei/port-in-use-by-emacs 38080))
;;     (org-roam-server-mode))
;;   (smartparens-global-mode 1))

(use-package! org-ref
  :after org-roam
  :config
  (let ((bib-files
         (mapcar (lambda (f)
                   (expand-file-name f zwei/org-roam-bib-directory))
                 zwei/org-roam-bib-files)))
    (setq reftex-default-bibliography bib-files
          org-ref-default-bibliography bib-files
          bibtex-completion-bibliography bib-files
          org-cite-global-bibliography bib-files
          org-ref-bibliography-notes zwei/org-roam-bib-directory
          bibtex-completion-notes-path zwei/org-roam-bib-directory
          org-ref-pdf-directory zwei/org-roam-bib-files-directory
          bibtex-completion-library-path `(,zwei/org-roam-bib-files-directory)
          org-ref-completion-library 'org-ref-ivy-cite
          ;; Rules for automatic key gen
          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 5
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5))

  (org-ref-ivy-cite-completion)

  (require 'org-ref-url-utils)
  (require 'org-ref-isbn)

  ;; Mappings for org-ref
  (map! :map bibtex-mode-map
        :localleader
        ;; Nav
        :desc "Next entry" "j" #'org-ref-bibtex-next-entry
        :desc "Prev entry" "k" #'org-ref-bibtex-previous-entry
        ;; Open
        :desc "Open browser" "b" #'org-ref-open-in-browser
        :desc "Open roam entry" "r" #'zwei/bibtex-open-roam-at-point
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

  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "roam")
         :desc "Insert citation" "c" #'zwei/bibtex-actions-insert-org-ref-citation
         :desc "Open citation roam entry" "RET" #'zwei/org-roam-open-citation-roam-entry))

  (map! :after markdown
        :map markdown-mode-map
        :localleader
        :desc "Insert citation" "c" #'org-ref-insert-link))

(use-package! bibtex-actions
  :commands (zwei/bibtex-actions-insert-org-ref-citation))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :commands (zwei/bibtex-open-roam-at-point
             zwei/bib+ref+roam-book-title
             zwei/org-roam-open-citation-roam-entry)
  :config
  (require 'org-ref)
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
          "url")
        orb-templates
        `(("r" "ref" plain #'org-roam-capture--get-point ""
           :file-name ,(concat zwei/org-roam-bib-directory "/%<%Y%m%d%H%M%S>-${slug}")
           :head
           ,(concat "#+TITLE: ${title} by ${author-abbrev}\n"
                    "#+ROAM_ALIAS: \n"
                    "#+ROAM_KEY: ${ref}\n"
                    "#+ROAM_TAGS: \n"
                    "\n"
                    "- related :: \n"
                    "\n"
                    "* ${title}\n"
                    ":PROPERTIES:\n"
                    ":Custom_ID: ${citekey}\n"
                    ":AUTHOR: ${author}\n"
                    ":KEYWORDS: ${keywords}\n"
                    ":END:\n"
                    "* Notes:\n"
                    "- ")
           :immediate-finish t
           :unnarrowed t)))

  ;; Mappings
  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "roam")
         :desc "Open citation roam entry" "B" #'zwei/org-roam-open-citation-roam-entry
         :desc "Create book bib+roam" "C" #'zwei/bib+ref+roam-book-title)))

(use-package! anki-editor
  :after org-roam
  :defer t
  :config
  (setq anki-editor-anki-connect-listening-port 38040)
  (setq anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p)))))

(use-package! kindle-highlights-to-org
  :after org-roam
  :defer t)


;;; +roam.el ends here
