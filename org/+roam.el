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
(require 'org-roam-graph)
(require 'org-ref)

;; Variables
(defconst zwei/slip-boxes
  '(("d" "[d]efault --permanent" "")
    ("b" "[b]ib -- literature" "bib/")
    ("p" "[p]osts" "posts/")
    ("l" "[l]ife" "life/")
    ("w" "[w]ork" "work/"))
  "Zettelkasten slip boxes in (key name dir) format.")

;; Patch in slip box tagging
(cl-defmethod org-roam-node-filetitle ((node org-roam-node))
  "Return the file TITLE for the node."
  (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

(cl-defmethod org-roam-node-directories ((node org-roam-node))
  (if-let ((dirs (file-name-directory
                  (file-relative-name (org-roam-node-file node)
                                      org-roam-directory))))
      (format "(%s)" (string-join (f-split dirs) "/"))
    " "))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy for the node."
  (let ((title (org-roam-node-title node))
        (olp (org-roam-node-olp node))
        (level (org-roam-node-level node))
        (filetitle (org-roam-node-filetitle node)))
    (concat
     (if (> level 0) (concat filetitle " > "))
     (if (> level 1) (concat (string-join olp " > ") " > "))
     title)))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                        :from links
                        :where (= dest $s1)
                        :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))

;; Functions
(defun zwei/org-roam-ui-dirs-to-tags (args)
  "Add subdirectories as tags to roam nodes. ARGS same as `websocket-send-text'.
Advise `websocket-send-text' using `:filter-args' combinator to use.
Will only change the output if using the `oru-ws' websocket and the text arg is
of type `graphdata' (coming from `org-roam-ui--send-graphdata')."
  (let* ((websocket (nth 0 args))
         (raw-json-text (nth 1 args))
         (raw-text (json-read-from-string raw-json-text))
         (type-cons (assoc 'type raw-text)))
    (if (and (eq websocket oru-ws)
             (string= (cdr type-cons) "graphdata"))
        (let* ((response (cdr (assoc 'data raw-text)))
               (nodes (cdr (nth 0 response)))
               (tags (nth 2 response)))
          (when (vectorp (cdr tags))
            (setq tags `(tags ,(aref (cdr tags) 0))))
          (setf (cdr (elt response 0))
                (mapcar
                 (lambda (node)
                   (let* ((node-tags (assoc 'tags node))
                          (properties (cdr (assoc 'properties node)))
                          (file-name (cdr (assoc 'FILE properties)))
                          (dir-tag
                           (if-let
                               ((dirs (file-name-directory
                                       (file-relative-name file-name
                                                           org-roam-directory))))
                               (format "%s" (string-join (f-split dirs) "/")))))
                     (when (vectorp (cdr node-tags))
                       (setq node-tags `(tags ,(aref (cdr node-tags) 0))))
                     (when dir-tag
                       (if (assoc 'tags node-tags)
                           (setq node-tags (append node-tags `(,dir-tag)))
                         (setq node-tags `(tags ,dir-tag)))
                       (add-to-list 'tags dir-tag t))
                     (setf (alist-get 'tags node) (cdr node-tags))
                     node))
                 nodes))
          (setf (elt response 2) tags)
          `(,oru-ws ,(json-encode `((type . "graphdata") (data . ,response)))))
      args)))

(defun zwei/roam-rename (new-name)
  "Move current file to NEW-NAME. `org-roam' takes care of adjusting all links."
  (interactive
   (list (let ((filename (buffer-file-name)))
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
      (orb-edit-note citekey))))

(defun zwei/org-roam-open-citation-roam-entry ()
  "Open roam entry related to citation under cursor."
  (interactive)
  (let ((citekey (org-ref-get-bibtex-key-under-cursor)))
    (if (not citekey)
        (message "Citekey not found.")
      (orb-edit-note citekey))))

(defun filter-out-p (str)
  "Filter out <p> tags from STR when exporting Anki notes."
  (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                            "" str))

(defun zwei/add-anki-note ()
  "Open temp org buffer to add an anki note."
  (interactive)
  (+popup-buffer (get-buffer-create "*anki*") '(:select t))
  (select-window (get-buffer-window "*anki*"))
  (org-mode)
  (insert "* Anki cards to add:\n")
  (anki-editor-insert-note))


;; Config
(setq org-roam-node-display-template
      "${directories:7} ${hierarchy:*} ${tags:10} ${backlinkscount:-3}"
      org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section)
      org-roam-capture-templates
      (mapcar (lambda (x)
                (let ((key  (nth 0 x))
                      (name (nth 1 x))
                      (dir  (nth 2 x)))
                  `(,key ,name
                         plain
                         "%?"
                         :target (file+head
                                  ,(concat dir "%<%Y%m%d%H%M%S>-${slug}" ".org")
                                  ,(concat "#+TITLE: ${title}\n"
                                           "- related :: \n"
                                           "\n"
                                           "*  "))
                         :immediate-finish t
                         :unnarrowed t)))
              zwei/slip-boxes)
      org-roam-capture-ref-templates
      '(("r" "ref (capture)"
         plain
         "%?"
         :target (file+head
                  ,(concat "bib/" "%<%Y%m%d%H%M%S>-${slug}" ".org")
                  ,(concat ":PROPERTIES\n"
                           ":ROAM_REFS: %{ref}\n"
                           ":END:\n"
                           "#+TITLE: ${title}\n"
                           "- related :: \n"
                           "\n"
                           "*  Notes\n"
                           "- "))
         :immediate-finish t
         :unnarrowed t))
      org-roam-graph-viewer (pcase (zwei/which-linux-distro)
                              ("Arch" "/usr/bin/chromium")
                              (_ nil)))

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
               :unnarrowed t)
             t)

;; Mappings
(map! :map org-mode-map
      :localleader
      (:prefix ("m" . "roam")
       :desc "Delete file" "D" #'zwei/roam-delete
       :desc "Rename file" "R" #'zwei/roam-rename
       :desc "Move slipbox" "M" #'zwei/roam-move-to-slip-box
       (:prefix ("a" . "anki")
        :desc "Add a note" "a" #'zwei/add-anki-note)))


(use-package! org-ref
  :after org-roam
  :config
  (let ((bib-files
         (mapcar (lambda (f)
                   (expand-file-name f zwei/org-roam-bib-directory))
                 zwei/org-roam-bib-files)))
    (setq reftex-default-bibliography bib-files
          bibtex-completion-bibliography bib-files
          org-cite-global-bibliography bib-files
          bibtex-completion-notes-path zwei/org-roam-bib-directory
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
         :desc "Insert citation" "c" #'bibtex-actions-insert-citation
         :desc "Open citation roam entry" "RET" #'zwei/org-roam-open-citation-roam-entry))

  (map! :after markdown
        :map markdown-mode-map
        :localleader
        :desc "Insert citation" "c" #'org-ref-insert-link))

(use-package! bibtex-actions
  :after org-roam
  :defer t)

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

  ;; mappings
  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "roam")
         :desc "open citation roam entry" "b" #'zwei/org-roam-open-citation-roam-entry
         :desc "create book bib+roam" "c" #'zwei/bib+ref+roam-book-title)))

(use-package! websocket
  :commands (org-roam-ui-mode)
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode)
  :init
  (advice-add 'websocket-send-text :filter-args #'zwei/org-roam-ui-dirs-to-tags)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
        org-roam-ui-port 38080
        org-roam-ui-find-ref-title t
        org-roam-ui-retitle-ref-nodes t))

(use-package! anki-editor
  :after org-roam
  :defer t
  :commands (anki-editor-insert-note
             anki-editor-push-notes
             anki-editor-cloze-region
             anki-editor-convert-region-to-html
             zwei/add-anki-note)
  :config
  (anki-editor-mode t)
  (setq anki-editor-anki-connect-listening-port 38040
        anki-editor-create-decks nil
        anki-editor-org-tags-as-anki-tags nil
        anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p))))
  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "roam")
         (:prefix ("a" . "anki")
          :desc "Push notes" "p" #'anki-editor-push-notes
          :desc "Insert note" "i" #'anki-editor-insert-note
          :desc "Cloze region" "c" #'anki-editor-cloze-region
          :desc "Convert region to HTML" "h" #'anki-editor-convert-region-to-html))))

(use-package! kindle-highlights-to-org
  :after org-roam
  :defer t)

;;; +roam.el ends here
