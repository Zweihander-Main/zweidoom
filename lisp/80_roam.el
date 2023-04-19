;;; 80_roam --- org-roam config -*-lexical-binding:t-*-
;;;
;;; Commentary:
;;;
;;; Contains roam configuration. Pretty much all slipbox config is here.
;;;
;;; Code:

(map! :map org-mode-map
      :localleader
      (:prefix ("m" . "roam")
       :desc "Delete file" "D" #'zwei/roam-delete
       :desc "Rename file" "R" #'zwei/roam-rename
       :desc "Move slipbox" "M" #'zwei/roam-move-to-slip-box))

(eval-when-compile
  (declare-function f-join "f"))

(defconst zwei/slip-boxes
  '(("d" "[d]efault --permanent" "")
    ("b" "[b]ib -- literature" "bib/")
    ("p" "[p]osts" "posts/")
    ("l" "[l]ife" "life/")
    ("w" "[w]ork" "work/"))
  "Zettelkasten slip boxes in (key name dir) format.")

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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


(use-package! org-roam
  :commands (zwei/roam-delete
             zwei/roam-rename
             zwei/roam-move-to-slip-box)
  :config
  (eval-when-compile
    (declare-function org-collect-keywords "org")
    (declare-function f-split "f")
    (declare-function org-roam-db-query "org-roam")
    (declare-function org-roam-backlinks-section "org-roam")
    (declare-function org-roam-reflinks-section "org-roam")
    (declare-function org-roam-unlinked-references-section "org-roam"))

  ;; Override methods for display template
  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file title for the NODE from ORG-ROAM-NODE."
    (with-temp-buffer (insert-file-contents (org-roam-node-file node) nil 0 512)
                      (car (last (car (org-collect-keywords '("TITLE")))))))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    "Add directories to NODE from ORG-ROAM-NODE."
    (if-let ((dirs (file-name-directory
                    (file-relative-name (org-roam-node-file node)
                                        org-roam-directory))))
        (format "(%s)" (string-join (f-split dirs) "/"))
      " "))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the NODE from ORG-ROAM-NODE."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-filetitle node)))
      (concat
       (if (> level 0) (concat filetitle " > "))
       (if (> level 1) (concat (string-join olp " > ") " > "))
       title)))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    "Get backlinks count for NODE from ORG-ROAM-NODE."
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  ;; Config
  (setq org-roam-node-display-template
        "${directories:7} ${hierarchy:*} ${tags:10} ${backlinkscount:-3}"
        org-roam-mode-sections
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
                zwei/slip-boxes)))

;;; 80_roam ends here
