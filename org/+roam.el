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
    ("l" "literature" "biblio/"))
  "Zettelkasten slip boxes in (key name dir) format.")

(setq  org-roam-tag-sources '(prop all-directories)
       org-roam-index-file (concat org-roam-directory "/20200724000434-index.org")
       org-roam-capture-templates
       '(("d" "default"
          plain
          (function org-roam-capture--get-point)
          "%?"
          :file-name "%<%Y%m%d%H%M%S>-${slug}"
          :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#+ROAM_TAGS: \n- related :: \n\n* "
          :unnarrowed t))
       org-roam-capture-ref-templates
       '(("r" "ref"
          plain
          (function org-roam-capture--get-point)
          "%?"
          :file-name "websites/${slug}"
          :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}\n- related :: \n\n* "
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
          bibtex-completion-library-path `(,zwei/org-roam-bib-files-directory))))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-templates
        `(("r" "ref" plain #'org-roam-capture--get-point ""
           :file-name ,(concat zwei/org-roam-bib-directory "/${slug}")
           :head
           ,(concat "#+TITLE: ${title}\n"
                    "#+ROAM_KEY: ${ref}\n")
           :immediate-finish t
           :unnarrowed t))))

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
