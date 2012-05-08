;;; find2-projects.el --- find or project directory for find2.el

(defvar find2-project-files '(".git" ".hg" ".svn"))

(defvar find2-project-folders nil)

(defun find2-find-project ()
  (let ((path default-directory)
        (stop nil)
        (return-path nil))
    (while (not (or return-path stop))
      (dolist (file find2-project-files)
        (when (file-exists-p (concat path file))
          (setq return-path path)
          (return)))
      ;; root of the filesystem
      (if (string-match "^\\([a-z]:\\)?/$" path)
          (setq stop t))
      (setq path (file-name-directory (directory-file-name path))))
    return-path))

(add-hook 'find2-before-set-command-hook
          '(lambda()
             (dolist (dir find2-project-folders)
               (when (string-match (concat "^" (regexp-quote (file-name-as-directory dir))) default-directory)
                 (setq find2-project-root dir
                       find2-project-command nil)
                 (return)))
             (unless find2-project-root
               (setq find2-project-root (find2-find-project)))))

(provide 'find2-find-project)
