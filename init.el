;;; -*- lexical-binding: t -*-

;; Define helper command for reloading configuration
(defun meomacs-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (meomacs-load-config "laf" t)
  (meomacs-load-config "editor" t)
  (meomacs-load-config "private" t)
  (meomacs-load-config "writing" t)
  (meomacs-load-config "programming" t)
  (meomacs-load-config "addons" t))

;; Define helper command for open configuration file.
(defun meomacs-open-configuration ()
  "Open meomacs.org under `user-emacs-directory'."
  (interactive)
  (let ((config (completing-read "Open configuration: "
				 '("private"
				   "laf"
				   "editor"
				   "writing"
				   "programming"
				   "addons")
				 nil
				 t)))
    (find-file (expand-file-name (format "%s.org" config) user-emacs-directory))))

(global-set-key (kbd "<f9>") 'meomacs-open-configuration)
(global-set-key (kbd "<f12>") 'meomacs-refresh)

;; Load main configuration
(meomacs-load-config "editor")
(meomacs-load-config "writing")
(meomacs-load-config "programming")
(meomacs-load-config "addons" t)

;; Load fonts setup
(when window-system
  (meomacs-load-ext-font)
  (meomacs-load-face-font))

