(use-package org
  :ensure t
  :demand t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq
 org-catch-invisible-edits 'error
 org-highlight-latex-and-related '(latex)
 org-default-notes-file "~/Documents/Notes/Dispatch.org"
 org-agenda-files '("~/Documents/Notes/")
 org-clock-mode-line-total 'current
 org-capture-templates
 '(
   ("a" "Anything" entry
    (file "~/Documents/Notes/Dispatch.org")
    "* TODO %^{Title} (%t)\n\n%i%?"
    :empty-lines 1
    :kill-buffer t)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(use-package org-clock-csv
  :ensure t)
