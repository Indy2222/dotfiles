(defun indy/czech ()
  "Switch input method to Czech & use Czech Ispell dictionary."
  (interactive)
  (set-input-method "czech-qwerty")
  (ispell-change-dictionary "czech"))

(defun indy/portuguese ()
  "Switch input method to Portuguese & use Portuguese Ispell dictionary."
  (interactive)
  (set-input-method "portuguese-prefix")
  (ispell-change-dictionary "portugues"))

(defun indy/english ()
  "Switch input method to Czech & use Czech Ispell dictionary."
  (interactive)
  (deactivate-input-method)
  (ispell-change-dictionary "english"))

(defun indy/kill-file-name ()
  "Place buffer-file-name kill ring and display it."
  (interactive)
  (if buffer-file-name
      (progn (kill-new buffer-file-name)
             (message "Buffer file: %s" buffer-file-name))
    (error "Current buffer is not visiting a file!")))

(global-set-key "\C-cif" 'indy/kill-file-name)

(defun indy/date-time ()
    "Place current date-time to kill ring and display it."
    (interactive)
    (let ((date-time-string (format-time-string "%Y-%m-%d %T")))
      (kill-new date-time-string)
      (message "Current date-time: %s" date-time-string)))

(global-set-key "\C-cid" 'indy/date-time)
