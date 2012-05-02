;;; find2-svn.el --- svn project support for find2.el

(add-hook 'find2-custom-project-hook
          '(lambda ()
             (let ((svn-info (shell-command-to-string "svn info")))
               (when (string-match "Working Copy Root Path: \\([^\n]*\\)" svn-info)
                 (setq find2-project-root (match-string 1 svn-info)
                       find2-project-command "svn list -R %s")))))

(provide 'find2-svn)
