;;; find2-svn.el --- svn project support for find2.el

(add-hook 'find2-custom-project-hook
          '(lambda ()
             (unless find2-project-root
               (let ((svn-info (shell-command-to-string "svn info")))
                 (if (string-match "Working Copy Root Path: \\([^\n]*\\)" svn-info)
                     (setq find2-project-root (match-string 1 svn-info)))))))

(provide 'find2-svn)
