;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Code:
;;;early-init file

;; fix (void-function cl-defmacro)
(eval-when-compile
  (require 'cl-lib))

(defvar poly/debug nil
  "Debug flag.")

;; ;; Shut off message buffer.  To debug Emacs, comment these out so you can see
;; ;; output from message function calls.
;; (unless poly/debug
;;   (setq message-log-max nil)
;;   )

;; Check if message buffer exists before killing (not doing so errors
;; eval-buffer of an init file).
;; (when (get-buffer "*Messages*")
;;   (kill-buffer "*Messages*"))

;; Disable GC during initialization(for the case, early-init.el is not used)
(setq gc-cons-threshold most-positive-fixnum)

;; fix macOS 15 lose focus
(select-frame-set-input-focus (selected-frame))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq inhibit-splash-screen t)

;; Disable most GUI widgets early on
(setq default-frame-alist '(;; (alpha . (0.90 0.90))
			    ;; (top . 1)
			    ;; (left . 0)
			    (ns-appearance . dark)
			    (ns-transparent-titlebar . t)
			    (ns-use-proxy-icon . nil)
			    (mac-frame-tabbing . nil)
			    (drag-internal-border . 1)
			    ;; (drag-internal-border . 0)
			    (drag-with-tab-line . t)
			    (internal-border-width . 0)
			    ;; (internal-border-width . 5)
			    (vertical-scroll-bars . nil)
			    (horizontal-scroll-bars . nil)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)
			    (no-special-glyphs . t)
			    ;; (fullscreen . maximized)
			    (undecorated . t) ;; remove title bar
			    (undecorated-round . t)
			    ;; (font . "JetBrains Mono-14")
			    (line-spacing . 0.2)))

;; (frame-parameter nil 'width)
;; (frame-parameter nil 'height)

;; Set the initial screen location to be top-left
;; (setq initial-frame-alist
;;       (append
;;        (list
;;         ;; '(top . 0) '(left . 0)
;; 	;; '(width . 206)
;; 	;; '(height . 60)
;;         '(cursor-type . bar))
;;        ;; default-frame-alist
;;        ))
;; (setq initial-frame-alist (copy-alist default-frame-alist))

;; Ensure we have correct user-emacs-directory
;; The folder of meomacs can be placed anywhere, and started with
;;   emacs -q -l /path/to/meomacs/init.el
(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))

(setq url-proxy-services
      '(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10.*|192\\.168\\..*\\)")
        ("http" . "127.0.0.1:6152")
        ("https" . "127.0.0.1:6152")))

(setenv "HTTP_PROXY"  "http://127.0.0.1:6152")
(setenv "HTTPS_PROXY" "http://127.0.0.1:6152")

 ;; (setq url-proxy-services
 ;;      '(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10.*|192\\.168\\..*\\)")
 ;;        ("http" . "127.0.0.1:8888")
 ;;        ("https" . "127.0.0.1:8888")))

(defun poly/file-mod-time(file)
  "Get FILE mod time."
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
	;; (message "generate file, source: %s, taget: %s" source target)
	)
      (load-file target))))

;; Prepare private.org when not exist.
(unless (file-exists-p (expand-file-name "private.org" user-emacs-directory))
  (copy-file
   (expand-file-name "private_template.org" user-emacs-directory)
   (expand-file-name "private.org" user-emacs-directory)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if poly/debug
    (progn
      (setq warning-minimum-level :debug)
      (setq debug-on-error t)
      (setq stack-trace-on-error t))
  (progn
    ;; Not really liking that warnings create a buffer at the bottom.
    (setq warning-display-at-bottom nil)
    (setq warning-minimum-level :emergency)
    (setq debug-on-error nil)
    (setq stack-trace-on-error nil)))

;; switch off Emoji popup on M-C-Spc, M-h hiding all windows etc.
;; To turn off M-C-d dictionary popup, see https://stackoverflow.com/a/20692071:
;; - Edit ~/Library/Preferences/com.apple.symbolichotkeys.plist
;; - Find the code for kCGHotKeyLookUpWordInDictionary (70), and set
;;   `enabled' to OFF (if it's not there just create an entry `70' with
;;   `enabled' = OFF).
;; - Restart your system
;; Some mac-bindings interfere with Emacs bindings.
;; (when (boundp 'mac-pass-command-to-system)
  ;; disable system call to commands like
  ;; C-h (hide frame on macOS by default
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-control-to-system nil) ;;)

;; Native compilation settings
(when (featurep 'native-compile)
  (let ((poly-local-dir (expand-file-name ".local" user-emacs-directory)))
    ;; Silence compiler warnings as they can be pretty disruptive
    (setq native-comp-async-report-warnings-errors nil
	  native-comp-warning-on-missing-source nil)
    (setq native-comp-jit-compilation nil
          native-comp-enable-subr-trampolines nil)
    ;; ;; Make native compilation happens asynchronously
    ;; (setq native-comp-deferred-compilation t)

    ;; Set the right directory to store the native compilation cache
    ;; NOTE the method for setting the eln-cache directory depends on the emacs version
    (when (fboundp 'startup-redirect-eln-cache)
      (if (version< emacs-version "29")
          (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" poly-local-dir)))
	(startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" poly-local-dir)))))
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" poly-local-dir))))
