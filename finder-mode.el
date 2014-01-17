;;; finder-mode.el --- A file finder for Emacs, find files in project

;; Author: uedsky
;; Keywords: file finder

(defvar finder-default-dir ".")
(defvar finder-omit-extensions '("log" "localized" "DS_Store"))
(defvar finder-omit-files '(".git" ".svn" ".hg"))
(defvar finder-omit-regexp nil)
(defvar finder-project-files '(".git" ".hg" ".svn"))

(defvar finder-default-command
  (if (eq system-type 'windows-nt)
      "dir %s /-n /b /s /a-d"
    "find %s -type f"))
(defvar finder-projects (make-hash-table :test 'equal))
(defvar finder-completion-buffer-name "*Finder Completions*")

(defun finder-shell-no-eof (cmd)
  (let ((output (shell-command-to-string cmd)))
    (substring output 0 -1)))

(defun finder-get-project-root ()
  (setq finder-project-root nil
        finder-project-command finder-default-command)
  (cond ((equal (finder-shell-no-eof "git rev-parse --is-inside-work-tree") "true")
         (setq finder-project-root (finder-shell-no-eof "git rev-parse --show-toplevel")
               finder-project-command "cd %s && git ls-files"))
        ((let ((finder-project-root (finder-shell-no-eof "hg root || echo __error__")))
           (if (string-match "__error__" finder-project-root)
               (setq finder-project-root nil)
             t))
         (setq finder-project-command "hg --cwd %s locate"))
        ;; find project root by checking finder-project-files's exsitence
        (t (let ((path default-directory))
             (while (and (not finder-project-root) path)
               (dolist (file finder-project-files)
                 (when (file-exists-p (concat path file))
                   (setq finder-project-root path)
                   (return)))
               ;; root of the filesystem
               (if (string-match "^\\([a-z]:\\)?/$" path)
                   (setq path nil)
                 (setq path (file-name-directory (directory-file-name path))))))))

  (unless finder-project-root
    (setq finder-project-root (expand-file-name finder-default-dir)))
  
  (setq finder-project-command (format finder-project-command (shell-quote-argument finder-project-root))))

(defun finder-get-project-files ()
  (interactive)
  (let ((output (shell-command-to-string finder-project-command))
        (omit-extensions (concat "\\.\\(" (mapconcat 'regexp-quote finder-omit-extensions "\\|") "\\)$"))
        (omit-files (concat "\\(/\\|^\\)\\(" (mapconcat 'regexp-quote finder-omit-files "\\|") "\\)\\(/\\|$\\)"))
        (filelist nil))
    ;; preprocessing the directory separator for `dir' command in windows
    (if (and (eq system-type 'windows-nt)
             (equal (substring finder-project-command 0 3) "dir"))
        (setq output (replace-regexp-in-string "\\\\" "/" output)))
    (dolist (path (split-string output "\n"))
      (or (string-match omit-files path)
          (string-match omit-extensions path)
          (and finder-omit-regexp (string-match finder-omit-regexp path))
          (setq filelist (cons (list path (file-name-nondirectory path)) filelist))))
    (puthash finder-project-root filelist finder-projects)))

(defun finder-quote (str)
  (replace-regexp-in-string " " ".*" (regexp-quote str)))

(defun finder-command-hook ()
  (let* ((query (finder-quote (minibuffer-contents-no-properties)))
        (only-file (if (string-match "/\\|\\.\\*" query) 0 1))
        (filelist nil))
    (get-buffer-create finder-completion-buffer-name)
    (set-buffer finder-completion-buffer-name)
    (erase-buffer)
    (dolist (file (gethash finder-project-root finder-projects))
      (if (string-match query (elt file only-file))
        (setq filelist (cons (car file) filelist))))
    (insert (mapconcat 'identity filelist "\n"))
    (setq finder-selected-index nil)
    (goto-char (point-min))
    (if (eq (point-min) (point-max))
        (insert "No matched file!")
      (setq finder-selected-index 1)
      (finder-highlight))
    (display-buffer finder-completion-buffer-name)))

(setq finder-initialized nil)
(defun finder-init ()
  (unless finder-initialized
    (setq finder-keymap (make-sparse-keymap))
    (set-keymap-parent finder-keymap minibuffer-local-map)
    (define-key finder-keymap "\C-n" 'finder-next)
    (define-key finder-keymap "\C-p" 'finder-previous)
    (define-key finder-keymap [down] 'finder-next)
    (define-key finder-keymap [up] 'finder-previous)
    (define-key finder-keymap "\C-r" 'finder-get-project-files)
    (define-key finder-keymap "\r" 'finder-select)

    (setq finder-initialized t)))

;;;###autoload
(defun finder-mode ()
  (interactive)

  (finder-init)
  (setq finder-mode t)
  (finder-get-project-root)
  (unless (gethash finder-project-root finder-projects)
    (finder-get-project-files))
  (add-hook 'minibuffer-setup-hook 'finder-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'finder-minibuffer-exit)
  (read-string ">>> ")
  (if finder-selected-file
      (find-file finder-selected-file)))

(defun finder-next ()
  (interactive)
  (finder-highlight 1))

(defun finder-previous ()
  (interactive)
  (finder-highlight -1))

(setq finder-selected-file nil)
(defun finder-select ()
  (interactive)
  (when finder-selected-index
    (set-buffer finder-completion-buffer-name)
    (goto-char (line-beginning-position))
    (delete-char 2)
    (setq finder-selected-file (finder-getline))
    (unless (string-match "^/\\|^[a-z]:" finder-selected-file)
      (setq finder-selected-file (concat (file-name-as-directory finder-project-root) finder-selected-file)))
    (exit-minibuffer)))

(defun finder-getline ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun finder-highlight (&optional delta)
  (when finder-selected-index
    ;; (save-excursion
    (set-buffer finder-completion-buffer-name)
    (when delta
      (goto-line finder-selected-index)
      (goto-char (line-beginning-position))
      (delete-char 2)
      (remove-text-properties (line-beginning-position) (line-end-position) '(face nil))
      (setq finder-selected-index (+ finder-selected-index delta))
      (let ((lines-count (count-lines (point-min) (point-max))))
        (if (< finder-selected-index 1)
            (setq finder-selected-index (+ lines-count finder-selected-index)))
        (if (> finder-selected-index lines-count)
            (setq finder-selected-index (- finder-selected-index lines-count))))
      (goto-line finder-selected-index)
      (set-window-point (get-buffer-window finder-completion-buffer-name) (point)))
    (goto-char (line-beginning-position))
    (insert "> ")
    (add-text-properties (line-beginning-position) (line-end-position) '(face isearch))
    ;; )
    ))

(defun finder-minibuffer-setup ()
  (when finder-mode
    (add-hook 'post-command-hook 'finder-command-hook nil t)
    (use-local-map finder-keymap)))

(defun finder-minibuffer-exit ()
  (when finder-mode
    (use-local-map (keymap-parent finder-keymap))
    (setq finder-mode nil)))

(provide 'finder-mode)
