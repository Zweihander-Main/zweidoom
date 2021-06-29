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

(defconst zwei/slip-boxes
  '(("p" "permanent" "")
    ("l" "literature" "bib/"))
  "Zettelkasten slip boxes in (key name dir) format.")

(setq  org-roam-tag-sources '(prop all-directories)
       org-roam-index-file (concat org-roam-directory "/20200724000434-index.org")
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
                                   "* ")
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
                   "* ")
          :immediate-finish t
          :unnarrowed t))
       org-roam-graph-viewer (pcase (zwei/which-linux-distro)
                               ("Arch" "/usr/bin/chromium")
                               (_ nil)))

(use-package! org-roam-server
  :after-call org-roam-server-mode
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 38080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files t
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil  ; look into
        org-roam-server-cite-edge-dashes t
        org-roam-server-extra-cite-edge-options nil
        org-roam-server-style nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; Prevent doom error with smartparens and stop multiple instances
(after! org-roam
  (smartparens-global-mode -1)
  (unless (or org-roam-server-mode
              (zwei/port-in-use-by-emacs 38080))
    (org-roam-server-mode))
  (smartparens-global-mode 1))

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
          org-ref-bibliography-notes zwei/org-roam-bib-directory
          bibtex-completion-notes-path zwei/org-roam-bib-directory
          org-ref-pdf-directory zwei/org-roam-bib-files-directory
          bibtex-completion-library-path `(,zwei/org-roam-bib-files-directory)
          ;; Rules for automatic key gen
          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 5
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5))

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
         :desc "Lookup ISBN" "i" #'isbn-to-bibtex-lead))

  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "roam")
         :desc "Insert citation" "c" #'org-ref-insert-link))

  (map! :after markdown
        :map markdown-mode-map
        :localleader
        :desc "Insert citation" "c" #'org-ref-insert-link))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
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
           :file-name ,(concat zwei/org-roam-bib-directory "/$%<%Y%m%d%H%M%S>-{slug}")
           :head
           ,(concat "#+TITLE: ${title} by ${author-or-editor}\n"
                    "#+ROAM_ALIAS: \n"
                    "#+ROAM_KEY: ${ref}\n"
                    "#+ROAM_TAGS: \n"
                    "\n"
                    "- related :: \n"
                    "\n"
                    "* ${title}\n"
                    ":PROPERTIES:\n"
                    ":Custom_ID: ${citekey}\n"
                    ":URL: ${url}\n"
                    ":AUTHOR: ${author-or-editor}\n"
                    ":KEYWORDS: ${keywords}"
                    ":END:\n"
                    "* ")
           :immediate-finish t
           :unnarrowed t))
        org-ref-completion-library 'org-ref-ivy-cite))

(use-package! anki-editor
  :after org-roam
  :defer t
  :config
  (setq anki-editor-anki-connect-listening-port 38040)
  (defun filter-out-p (str)
    "Filter out <p> tags from STR when exporting Anki notes."
    (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                              "" str))
  (setq anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p)))))

(use-package! kindle-highlights-to-org
  :after org-roam
  :defer t)


;;; +roam.el ends here
