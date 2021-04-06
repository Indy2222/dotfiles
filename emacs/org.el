(use-package org
  :ensure t
  :demand t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq
 org-catch-invisible-edits 'error
 org-highlight-latex-and-related '(latex)
 org-default-notes-file "~/notes/notes.org"
 org-agenda-files '("~/notes/")
 org-capture-templates '(("t" "TODO" entry
                          (file "~/notes/todo.org")
                          "* TODO %?\n\n%a\n%i\n")
                         ("d" "TODO datamole" entry
                          (file "~/notes/todo-dm.org")
                          "* TODO %?\n\n%a\n%i\n")
                         ("n" "Notes" entry
                          (file "~/notes/notes.org")
                          "* %? %U\n\n%a\n%i\n")))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/Documents/bibliography/bibliography.bib")
        org-ref-default-bibliography "~/Documents/bibliography/bibliography.bib"
        org-ref-pdf-directory "~/Documents/bibliography/pdfs/"
        org-ref-bibliography-notes nil
        org-ref-completion-library 'org-ref-ivy-cite))

(use-package org-clock-csv
  :ensure t)
