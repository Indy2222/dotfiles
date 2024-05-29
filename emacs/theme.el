(setq-default mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-modified
        mode-line-remote
        " "
        mode-line-position
        " "
        mode-line-buffer-identification
        " "
        mode-line-misc-info
        mode-line-end-spaces))

(defun indy/mode-line-faces ()
  "Configure mode line faces."
  (let ((faces '(mode-line
                 mode-line-buffer-id
                 mode-line-emphasis
                 mode-line-highlight
                 mode-line-inactive)))
    (mapc (lambda (face) (set-face-attribute face nil :height 120)) faces)))

(defun indy/dark ()
  "Enable a dark theme and disable all other themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (enable-theme 'solarized-dark)
  (indy/mode-line-faces))

(defun indy/light ()
  "Enable a light theme and disable all other themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (enable-theme 'solarized-light)
  (indy/mode-line-faces))

(use-package solarized-theme
  :straight t
  :config
  (setq
   x-underline-at-descent-line t
   solarized-use-variable-pitch nil
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0
   solarized-distinct-fringe-background t
   solarized-high-contrast-mode-line nil)
  (load-theme 'solarized-light t nil)
  (load-theme 'solarized-dark t nil)
  (indy/light))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(set-fontset-font t nil (font-spec :height 120 :name "Noto Color Emoji"))
