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

;; (setq warning-minimum-level :debug)
;; (setq debug-on-error t)
;; (setq stack-trace-on-error t)

(setq warning-minimum-level :emergency)
(setq debug-on-error nil)

;; Native compilation settings
(when (featurep 'native-compile)
  (let ((poly-local-dir (expand-file-name ".local" user-emacs-directory)))
    ;; Silence compiler warnings as they can be pretty disruptive
    (setq native-comp-async-report-warnings-errors nil)

    ;; Make native compilation happens asynchronously
    (setq native-comp-deferred-compilation t)

    ;; Set the right directory to store the native compilation cache
    ;; NOTE the method for setting the eln-cache directory depends on the emacs version
    (when (fboundp 'startup-redirect-eln-cache)
      (if (version< emacs-version "29")
          (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" poly-local-dir)))
	(startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" poly-local-dir)))))

    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" poly-local-dir))))
