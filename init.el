;;; -*- lexical-binding: t -*-

;; Define helper command for reloading configuration
(defun poly-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (poly-load-org-config "private" t)
  (poly-load-org-config "laf" t)
  (poly-load-org-config "editor" t)
  (poly-load-org-config "writing" t)
  (poly-load-org-config "programming" t)
  (poly-load-org-config "addons" t))

;; Define helper command for open configuration file.
(defun poly-open-configuration ()
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

(global-set-key (kbd "<f9>") 'poly-open-configuration)
(global-set-key (kbd "<f12>") 'poly-refresh)

;; Load main configuration

(poly-load-org-config "project")
(poly-load-org-config "complete")
(poly-load-org-config "editor")
(poly-load-org-config "writing")
(poly-load-org-config "programming")
(poly-load-org-config "addons" t)
