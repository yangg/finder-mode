
(defvar lookup-limit 9999
  "limit output of file names for locate")

(defvar lookup-dir "~/")

(defvar lookup-omit-extensions ["jpg" "gif" "png" "log" "dat"])

(defvar lookup-omit-dirs [".git" ".svn" ".hg" "images" "log"])

(defvar lookup-completion-buffer-name "*Lookup Completions*")

(defun lookup-get-filelist(process output)
  (set-buffer lookup-completion-buffer-name)
  (erase-buffer)
  ;; (insert lookup-filelist)
  (let ((omit-extensions (concat "\\.\\(" (mapconcat 'regexp-quote lookup-omit-extensions "\\|") "\\)$"))
        (omit-dirs (concat "/\\(" (mapconcat 'regexp-quote lookup-omit-dirs "\\|") "\\)/"))
        (filename nil)
        (filelist []))
    (dolist (path (split-string output "\n"))
      (or (string-match omit-extensions path)
          (string-match omit-dirs path)
          (progn
            (setq filename (substring path (+ (or (search "/" path :from-end t) -1) 1)))
            (setq lookup-filelist (vconcat lookup-filelist (vector (list path filename))))
            (setq filelist (append filelist (list path)))
            )))
    (insert (mapconcat 'identity filelist "\n"))))

(defun lookup-quote (str)
  (replace-regexp-in-string "\\\\\\*" "[^/]*"
                            (replace-regexp-in-string "\\\\\\*\\\\\\*" ".*" (regexp-quote str))))

;; (setq query "/aaa.*aa")
;; (setq query "aa")
;; (or (and (or (search "/" query) (search "\\.\\*" query)) 0) 1)

(defun lookup-command-hook ()
  (let ((query (lookup-quote (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))
        (only-file nil))
    (setq only-file (or (and (or (search "/" query) (search ".*" query)) 0) 1))
    (get-buffer-create lookup-completion-buffer-name)
    (set-buffer lookup-completion-buffer-name)
    (erase-buffer)
    ;; (insert (format "%s\t%s\t" query only-file))
    (mapc '(lambda(file)
             (let ((path (elt file 0)))
               (when (string-match query (elt file only-file))
                 (insert (concat path "\n")))
               )) lookup-filelist)
    (setq lookup-file-selected-index 1)
    (goto-char (point-min))
    (lookup-file-highlight)
    (display-buffer lookup-completion-buffer-name)))

(setq lookup-file-initialized nil)

;;;###autoload
(defun lookup-file ()
  (interactive)

  (when (not lookup-file-initialized)
    (setq lookup-file-keymap (make-sparse-keymap))
    (set-keymap-parent lookup-file-keymap minibuffer-local-map)
    (define-key lookup-file-keymap "\C-n" 'lookup-file-next)
    (define-key lookup-file-keymap "\C-p" 'lookup-file-previous)
    (define-key lookup-file-keymap "\r" 'lookup-file-select)

    (setq lookup-dir (expand-file-name lookup-dir))
    ;; (setq lookup-process (start-process "lookup-file" nil "sh" "-c" (format "cd '%s' && git ls-files" lookup-dir)))
    ;; (setq lookup-process (start-process "lookup-file" nil "sh" "-c" (format "locate '%s' -l%s" lookup-dir lookup-limit)))
    (setq lookup-process (start-process "lookup-file" nil "sh" "-c" (concat "find " lookup-dir " -type f ")))
    (setq lookup-filelist [])
    (set-process-filter lookup-process 'lookup-get-filelist)
    (setq lookup-file-initialized t))

  (setq lookup-file-mode t)
  (add-hook 'minibuffer-setup-hook 'lookup-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'lookup-minibuffer-exit)
  (read-string "Lookup file: ")
  (if lookup-file-selected
      (find-file lookup-file-selected)))

(defun lookup-file-next ()
  (interactive)
  (lookup-file-highlight 1)
  )

(defun lookup-file-previous ()
  (interactive)
  (lookup-file-highlight -1)
  )

(setq lookup-file-selected nil)
(defun lookup-file-select ()
  (interactive)
  (set-buffer lookup-completion-buffer-name)
  (goto-char (line-beginning-position))
  (delete-char 2)
  (setq lookup-file-selected (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (exit-minibuffer)
  )

(defun lookup-file-highlight (&optional delta)
  ;; (save-excursion
  (set-buffer lookup-completion-buffer-name)
  (when delta
    ;; (insert (format "%s" lookup-file-selected-index))
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
    )
  (set-window-point (get-buffer-window lookup-completion-buffer-name) (point))
  (goto-char (line-beginning-position))
  (insert "> ")
  (add-text-properties (line-beginning-position) (line-end-position) '(face highlight))
  ;; )
  )

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
