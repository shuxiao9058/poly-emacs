;; Disable GC during initialization(for the case, early-init.el is not used)
(setq gc-cons-threshold most-positive-fixnum)

;; Ensure we have correct user-emacs-directory
;; The folder of meomacs can be placed anywhere, and started with
;;   emacs -q -l /path/to/meomacs/init.el
(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))

(defun poly/file-mod-time(file)
  (when (file-exists-p file)
    (nth 5 (file-attributes file))))

;; Define configuration loader helper
(defun poly-load-org-config (config-name &optional force-tangle)
  "Load configuration by CONFIG-NAME.
Firstly try load the CONFIG-NAME.el file.
If the elisp file was not found, tangle the CONFIG-NAME.org to generate one.

If FORCE-TANGLE is non-nil, always tangle before load."
  (let* ((source (expand-file-name (format "%s.org" config-name) user-emacs-directory))
	 (tangle-dir (expand-file-name "tangle" user-emacs-directory))
	 (target (expand-file-name (format "%s.el" config-name) tangle-dir))
         (target-mod-time (poly/file-mod-time target))
         (source-mod-time (poly/file-mod-time source)))
    (when (file-exists-p source)
      (make-directory tangle-dir t)
      (when (or force-tangle
                (not (file-exists-p target))
                (time-less-p target-mod-time source-mod-time))
	(require 'org)
	(require 'ob)
	(org-babel-tangle-file source target)
        (message "generate file, source: %s, taget: %s" source target))
      (load-file target))))

;; Prepare private.org when not exist.

(unless (file-exists-p (expand-file-name "private.org" user-emacs-directory))
  (copy-file
   (expand-file-name "private_template.org" user-emacs-directory)
   (expand-file-name "private.org" user-emacs-directory)))

(setq warning-minimum-level :debug)
(setq debug-on-error t)
(setq stack-trace-on-error t)

;; core configurations
(poly-load-org-config "core")
;; straight package
(poly-load-org-config "package")
(poly-load-org-config "ui")
(poly-load-org-config "keybindings")

;; Load configurations
(poly-load-org-config "private")
(poly-load-org-config "laf")
