;;; find2-xp.el --- fix directory separator for find2.el in windows xp

(add-hook 'find2-before-set-command-hook
          '(lambda ()
             (if (and (eq system-type 'windows-nt)
                      (equal find2-project-command "cd %s && git ls-files"))
                 (setq find2-project-root (replace-regexp-in-string "/" "\\\\" find2-project-root)))))

(provide 'find2-xp)
