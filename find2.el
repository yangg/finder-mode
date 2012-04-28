;;; find2.el --- A file finder for Emacs

;; Author: uedsky
;; Keywords: file finder

(defvar find2-dir "~")

(defvar find2-omit-extensions ["jpg" "gif" "png" "log" "localized" "DS_Store"])

(defvar find2-omit-files [".git" ".svn" ".hg" "images" "log"])

(defvar find2-omit-regexp nil)

(defvar find2-command "find %s -type f"
  "Available values are:
find %s -type f
(cd %s && git ls-files)")

(defvar find2-completion-buffer-name "*Find2 Completions*")

(defun find2-get-list()
  (interactive)
  (setq find2-dir (expand-file-name find2-dir)
        find2list [])
  (let ((output (shell-command-to-string (format find2-command find2-dir)))
        (omit-extensions (concat "\\.\\(" (mapconcat 'regexp-quote find2-omit-extensions "\\|") "\\)$"))
        (omit-files (concat "\\(/\\|^\\)\\(" (mapconcat 'regexp-quote find2-omit-files "\\|") "\\)\\(/\\|$\\)"))
        (filename nil))
    (dolist (path (split-string output "\n"))
      (or (string-match omit-files path)
          (string-match omit-extensions path)
          (and find2-omit-regexp (string-match find2-omit-regexp path))
          (setq filename (file-name-nondirectory path)
                find2list (vconcat find2list (vector (list path filename))))
          ))))

(defun find2-quote (str)
  (replace-regexp-in-string "\\\\\\*" "[^/]*"
                            (replace-regexp-in-string "\\\\\\*\\\\\\*" ".*" (regexp-quote str))))

(defun find2-command-hook ()
  (let* ((query (find2-quote (minibuffer-contents-no-properties)))
        (only-file (if (or (search "/" query) (search ".*" query)) 0 1))
        (filelist []))
    (get-buffer-create find2-completion-buffer-name)
    (set-buffer find2-completion-buffer-name)
    (erase-buffer)
    (mapc '(lambda(file)
             (let ((path (elt file 0)))
               (when (string-match query (elt file only-file))
                 (setq filelist (append filelist (list path)))
                 ))) find2list)
    (insert (mapconcat 'identity filelist "\n"))
    (setq find2-selected-index nil)
    (goto-char (point-min))
    (if (eq (point-min) (point-max))
        (insert "No matched file!")
      (setq find2-selected-index 1)
      (find2-highlight))
    (display-buffer find2-completion-buffer-name)))

(setq find2-initialized nil)
(defun find2-init ()
  (unless find2-initialized
    (setq find2-keymap (make-sparse-keymap))
    (set-keymap-parent find2-keymap minibuffer-local-map)
    (define-key find2-keymap "\C-n" 'find2-next)
    (define-key find2-keymap "\C-p" 'find2-previous)
    (define-key find2-keymap [down] 'find2-next)
    (define-key find2-keymap [up] 'find2-previous)
    (define-key find2-keymap "\C-r" 'find2-get-list)
    (define-key find2-keymap "\r" 'find2-select)

    (find2-get-list)
    (setq find2-initialized t)))

;;;###autoload
(defun find2 ()
  (interactive)

  (find2-init)
  (setq find2-mode t)
  (add-hook 'minibuffer-setup-hook 'find2-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'find2-minibuffer-exit)
  (read-string ">>> ")
  (if find2-selected-file
      (find-file find2-selected-file)))

(defun find2-next ()
  (interactive)
  (find2-highlight 1))

(defun find2-previous ()
  (interactive)
  (find2-highlight -1))

(setq find2-selected-file nil)
(defun find2-select ()
  (interactive)
  (when find2-selected-index
    (set-buffer find2-completion-buffer-name)
    (goto-char (line-beginning-position))
    (delete-char 2)
    (setq find2-selected-file (find2-getline))
    (unless (string-match "^/\\|^[a-z]:" find2-selected-file)
      (setq find2-selected-file (concat (file-name-as-directory find2-dir) find2-selected-file)))
    (exit-minibuffer)))

(defun find2-getline ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun find2-highlight (&optional delta)
  (when find2-selected-index
    ;; (save-excursion
    (set-buffer find2-completion-buffer-name)
    (when delta
      (goto-line find2-selected-index)
      (goto-char (line-beginning-position))
      (delete-char 2)
      (remove-text-properties (line-beginning-position) (line-end-position) '(face nil))
      (setq find2-selected-index (+ find2-selected-index delta))
      (let ((lines-count (count-lines (point-min) (point-max))))
        (if (< find2-selected-index 1)
            (setq find2-selected-index (+ lines-count find2-selected-index)))
        (if (> find2-selected-index lines-count)
            (setq find2-selected-index (- find2-selected-index lines-count))))
      (goto-line find2-selected-index)
      (set-window-point (get-buffer-window find2-completion-buffer-name) (point)))
    (goto-char (line-beginning-position))
    (insert "> ")
    (add-text-properties (line-beginning-position) (line-end-position) '(face highlight))
    ;; )
    ))

(defun find2-minibuffer-setup ()
  (when find2-mode
    (add-hook 'post-command-hook 'find2-command-hook nil t)
    (use-local-map find2-keymap)
    ))

(defun find2-minibuffer-exit ()
  (when find2-mode
    (use-local-map (keymap-parent find2-keymap))
    (setq find2-mode nil)))

(provide 'find2)
