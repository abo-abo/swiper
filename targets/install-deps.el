(setq melpa-stable (getenv "MELPA_STABLE"))
(setq package-user-dir
      (expand-file-name
       (format "~/.elpa/%s/elpa"
               (concat emacs-version (when melpa-stable "-stable")))))
(message "installing in %s ...\n" package-user-dir)
(package-initialize)
(setq package-archives
      (list (if melpa-stable
                '("melpa-stable" . "https://stable.melpa.org/packages/")
              '("melpa" . "http://melpa.org/packages/"))
            ;; '("gnu" . "http://elpa.gnu.org/packages/")
            ))
(package-refresh-contents)

(defconst ivy-dev-packages
  '(avy
    hydra
    wgrep))

(dolist (package ivy-dev-packages)
  (if (package-installed-p package)
      (message "%S: OK" package)
    (condition-case nil
        (progn
          (package-install package)
          (message "%S: ...OK" package))
      (error
       (message "%S: FAIL" package)))))

(save-window-excursion
  (package-list-packages t)
  (condition-case nil
      (progn
        (package-menu-mark-upgrades)
        (package-menu-execute t))
    (error
     (message "All packages up to date"))))
(provide 'targets/install-deps)
