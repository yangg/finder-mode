
(defvar lookup-dir "~")

(defvar lookup-omit-extensions ["jpg" "gif" "png" "log"])

(defvar lookup-omit-dirs [".git" ".svn" ".hg" "images" "log"])

(defvar lookup-command "find %s -type f"
  "Availables values are:
find %s -type f
cd %s && git ls-files
locate %s -l9999

create or update locate db in osx:
sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
sudo /usr/libexec/locate.updatedb
")

(defvar lookup-completion-buffer-name "*Lookup Completions*")

(defun lookup-get-filelist(process output)
  (setq lookup-file-text (concat lookup-file-text output))
  (when (eq (process-status lookup-process) 'exit)
    (let ((omit-extensions (concat "\\.\\(" (mapconcat 'regexp-quote lookup-omit-extensions "\\|") "\\)$"))
          (omit-dirs (concat "/\\(" (mapconcat 'regexp-quote lookup-omit-dirs "\\|") "\\)/"))
          (filename nil))
      (dolist (path (split-string lookup-file-text "\n"))
        (or (string-match omit-extensions path)
            (string-match omit-dirs path)
            (setq filename (file-name-nondirectory path)
                  lookup-filelist (vconcat lookup-filelist (vector (list path filename))))
            ))
      (lookup-command-hook))))

(defun lookup-quote (str)
  (replace-regexp-in-string "\\\\\\*" "[^/]*"
                            (replace-regexp-in-string "\\\\\\*\\\\\\*" ".*" (regexp-quote str))))

(defun lookup-command-hook ()
  (let ((query (lookup-quote (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))
        (only-file nil)
        (filelist []))
    (setq only-file (or (and (or (search "/" query) (search ".*" query)) 0) 1))
    (get-buffer-create lookup-completion-buffer-name)
    (set-buffer lookup-completion-buffer-name)
    (erase-buffer)
    (mapc '(lambda(file)
             (let ((path (elt file 0)))
               (when (string-match query (elt file only-file))
                 (setq filelist (append filelist (list path)))
                 ))) lookup-filelist)
    (insert (mapconcat 'identity filelist "\n"))
    (setq lookup-file-selected-index nil)
    (goto-char (point-min))
    (if (eq (point-min) (point-max))
        (insert "No matched file!")
      (setq lookup-file-selected-index 1)
      (lookup-file-highlight))
    (display-buffer lookup-completion-buffer-name)))

(setq lookup-file-initialized nil)
(defun lookup-init ()
  (unless lookup-file-initialized
    (setq lookup-file-keymap (make-sparse-keymap))
    (set-keymap-parent lookup-file-keymap minibuffer-local-map)
    (define-key lookup-file-keymap "\C-n" 'lookup-file-next)
    (define-key lookup-file-keymap "\C-p" 'lookup-file-previous)
    (define-key lookup-file-keymap "\r" 'lookup-file-select)

    (setq lookup-dir (expand-file-name lookup-dir))
    (setq lookup-process (start-process "lookup-file" nil "sh" "-c" (format lookup-command lookup-dir)))
    (setq lookup-file-text ""
          lookup-filelist [])
    (set-process-filter lookup-process 'lookup-get-filelist)
    (setq lookup-file-initialized t)))

;;;###autoload
(defun lookup-file ()
  (interactive)

  (lookup-init)
  (setq lookup-file-mode t)
  (add-hook 'minibuffer-setup-hook 'lookup-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'lookup-minibuffer-exit)
  (read-string "Lookup file: ")
  (if lookup-selected-file
      (find-file lookup-selected-file)))

(defun lookup-file-next ()
  (interactive)
  (lookup-file-highlight 1))

(defun lookup-file-previous ()
  (interactive)
  (lookup-file-highlight -1))

(setq lookup-selected-file nil)
(defun lookup-file-select ()
  (interactive)
  (when lookup-file-selected-index
    (set-buffer lookup-completion-buffer-name)
    (goto-char (line-beginning-position))
    (delete-char 2)
    (setq lookup-selected-file (lookup-getline))
    (unless (eq (elt lookup-selected-file 0) ?/)
      (setq lookup-selected-file (concat (file-name-as-directory lookup-dir) lookup-selected-file)))
    (exit-minibuffer)))

(defun lookup-getline ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lookup-file-highlight (&optional delta)
  (when lookup-file-selected-index
    ;; (save-excursion
    (set-buffer lookup-completion-buffer-name)
    (when delta
      (goto-line lookup-file-selected-index)
      (goto-char (line-beginning-position))
      (delete-char 2)
      (remove-text-properties (line-beginning-position) (line-end-position) '(face nil))
      (setq lookup-file-selected-index (+ lookup-file-selected-index delta))
      (let ((lines-count (count-lines (point-min) (point-max))))
        (if (< lookup-file-selected-index 1)
            (setq lookup-file-selected-index (+ lines-count lookup-file-selected-index)))
        (if (> lookup-file-selected-index lines-count)
            (setq lookup-file-selected-index (- lookup-file-selected-index lines-count))))
      (goto-line lookup-file-selected-index)
      (set-window-point (get-buffer-window lookup-completion-buffer-name) (point)))
    (goto-char (line-beginning-position))
    (insert "> ")
    (add-text-properties (line-beginning-position) (line-end-position) '(face highlight))
    ;; )
    ))

(defun lookup-minibuffer-setup ()
  (when lookup-file-mode
    (add-hook 'post-command-hook 'lookup-command-hook nil t)
    (use-local-map lookup-file-keymap)
    ))

(defun lookup-minibuffer-exit ()
  (when lookup-file-mode
    (use-local-map (keymap-parent lookup-file-keymap))
    (setq lookup-file-mode nil)))

(provide 'lookup-file)
