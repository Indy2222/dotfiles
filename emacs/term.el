(require 'term)

(defun indy/term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'indy/term-exec-hook)

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(defun indy/terminal ()
  "Create a new ZSH terminal buffer."
  (interactive)
  (let ((buffer-name nil)
        (buffer-num 1))
  (while (get-buffer (setq buffer-name (concat "term-" (number-to-string buffer-num))))
    (setq buffer-num (+ buffer-num 1)))
  (term "/bin/zsh")
  (rename-buffer buffer-name)))
