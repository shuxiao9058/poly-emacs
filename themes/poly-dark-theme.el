;;; -*- mode: emacs-lisp; eval: (rainbow-mode); lexical-binding: t -*-

;; Package-Requires: ((emacs "24.3"))

;;; Code:

(require 'cl-lib)

(deftheme poly-dark
  "My dark poly theme.")

;;;; Configuration options:

(defgroup poly-dark nil
  "Poly dark theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defvar poly-dark-theme-use-italic t
  "Non-nil means use italic for comment and docstring.")

(defvar poly-dark-theme-header-scales
  '(1.6 1.4 1.2 1.2 1.2 1.1 1.0)
  "Scales for headers.")

(defcustom poly-dark-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'poly-dark)

(defvar poly-dark-theme-main-color "#00AAAA"
  "The main color used for some places.
You may want to set this to window's border color.")

(defcustom poly-dark-alternate-mode-line-and-minibuffer t
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'poly-dark)

(defface tabnine-face '((t (:inherit font-lock-string-face)))
  "TabNine face")

;;
;; Assigment form: VARIABLE COLOR [TTY-COLOR]
(let ((colors '(
                (bg "#22212C" "#201F2E" nil) ;; Background
		(fg "#F8F8F2" "#F9F9F1" "brightwhite")   ;; Foreground
		(fg+1 "#CCCCCC" "#F9F9F1" "brightwhite")
		(fg-1 "#909090" "#F9F9F1" "brightwhite")

		(current "#454158" "#433D5C" "brightblack") ; Current-line/selection
		(comment "#7970A9" "#756AAF" "blue")        ; Comment
		(cyan "#80FFEA" "#86F9E6" "brightcyan")     ; Cyan
		(green "#8AFF80" "#8FF986" "green")         ; Green
		(orange "#FFCA80" "#F9C986" "brightred")    ; Orange
		(pink "#FF80BF" "#F986BF" "magenta")        ; Pink
		(purple "#9580FF" "#9986F9" "brightmagenta") ; Purple
		(red "#FF9580" "#F99986" "red")              ; Red
		(yellow "#FFFF80" "#F9F986" "yellow")        ; Yellow

		;; Other colors
		(bg2     "#201F2E" "#2B293D" "brightblack")
		(bg3      "#2B293D" "#35334D" "brightblack")
		(bg4      "#36334C" "#3F3D5C" "brightblack")
		(fg2      "#EDEDDE" "#EBEBE0" "brightwhite")
		(fg3      "#D6D6C2" "#D1D1C7" "white")
		(fg4      "#BABAAB" "#B3B3B3" "white")
		(alt-blue "#8A75F0" "#846EF7" "brightblue")
		(bg-1 "#17161D" "#3F3D5C" "brightblack") ;; mode-line
		(bg+1 "#454158" "#3F3D5C" "brightblack")
		(bg+2 "#303030" "#3F3D5C" "brightblack")
		(bg+3 "#404040" "#3F3D5C" "brightblack")
		(bg+4 "#505050" "#3F3D5C" "brightblack")
		(alt-blue "#8A75F0" "#846EF7" "brightblue")
		(bglighter "#393649" "#35334D" "brightblack")
		(bglight "#2E2B3B" "#35334D" "brightblack")
		(bgdark "#17161D" "#35334D" "brightblack")
		(bgdarker "#0B0B0F" "#3F3D5C" "brightblack")
		(black "#17161D" "#35334D" "brightblack")
		(white "#E0E0E0" "#B3B3B3" "white")
		(blue "#009F9F" "#846EF7" "brightblue")
		(green2 "#8AFF80" "#8FF986" "green")
		(light-purple "#B28CE2" "brightblue")
		(brown "#CFA300" "#F9C986" "brightred")
		(region "#454158" "#3F3D5C" "brightblack")
		(region2 "#350035" "#3F3D5C" "brightblack")
		(magenta "#ff05fa" "#F986BF" "magenta")))

      (faces '( ;; default
               (default :background ,bg :foreground ,fg)
               (default-italic :slant italic)
               (cursor                        :background ,white)
               (region                         :background ,region)
               (fringe                        :foreground ,fg4 :background ,bg)
               (show-paren-match               :underline ,green)
               (highlight                      :background ,bg+2)
               ;; `(highlight                      ((t (:background ,hilight-bg :foreground ,hilight-fg))))
               (button                         :foreground ,blue :underline t)
               (vertical-border               :foreground ,bg2)
               (window-divider                 :background ,bg+3)
               (window-divider-first-pixel     :foreground ,bg+1)
               (window-divider-last-pixel      :foreground ,bg+1)
               (line-number                    :foreground ,bg+3 :inherit default)
               (line-number-current-line       :foreground ,yellow)

               (parenthesis                    :foreground ,fg-1)
               ;; (completions-common-part        )
               ;; (minibuffer-prompt              ((t (quote (read-only nil cursor-intangible t face minibuffer-prompt));; ()
               ;;  				   )))
               (minibuffer-prompt
                ,@(if poly-dark-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground fg)
                    (list :weight 'bold :foreground pink)))
               ;; `(lazy-highlight                 ((t (:background ,bg+3))))
               ;; `(lazy-highlight                 ((t (:background nil))))
               ;; `(match                          ((t (:background ,bg+2))))
               (match :background ,yellow :foreground ,bg)
               (xref-match                     :inherit match)

               (secondary-selection           :background ,region2 :extend t)

               ;; ISearch
               (isearch                        :background ,green :foreground ,black)
               (isearch-fail                   :backgronud ,red :foreground ,orange)

               ;; Font Locks
               (font-lock-comment-face         :foreground ,comment :italic ,poly-dark-theme-use-italic)
               (font-lock-comment-delimiter-face        :foreground ,comment :italic ,poly-dark-theme-use-italic)
               (font-lock-string-face          :foreground ,yellow)
               (font-lock-doc-face             :foreground ,blue :italic ,poly-dark-theme-use-italic)
               (font-lock-builtin-face         :foreground ,purple)
               (font-lock-type-face            :forground ,green)
               (font-lock-variable-name-face   :foreground ,white)
               (font-lock-operator-face   :foreground ,red)
               (font-lock-keyword-face         :foreground ,red)
               (font-lock-number-face         :foreground ,purple)
               (font-lock-delimiter-face         :foreground ,white)
               (font-lock-bracket-face         :foreground ,red)
               (font-lock-escape-face         :foreground ,red)
               (font-lock-property-face         :foreground ,red)
               (font-lock-constant-face        :foreground ,purple)
               (font-lock-function-name-face   :foreground ,green2)
               (font-lock-warning-face         :foreground ,orange)
               (font-lock-preprocessor-face    :inherit font-lock-constant-face)

               (compilation-info               :inherit font-lock-function-name-face)
               (compilation-warning            :inherit font-lock-warning-face)
               (warning                        :inherit font-lock-warning-face)

               ;; IMenu
               ;; (imenu-list-entry-face-0          ((t ())))
               (imenu-list-entry-subalist-face-0  :bold t)

               ;; Mode Line
               (mode-line                      :background ,bg-1)
               (mode-line-inactive             :background ,bg+1)

               ;; Yascroll
               (yascroll:thumb-fringe          :background ,fg :foreground ,fg)
               (yascroll:thumb-text-area       :background ,fg :foreground ,fg)

               ;; Company
               (company-tooltip-common         :bold t)
               (company-tooltip-common-selection :bold t)
               (company-tooltip                :background ,bg+2)
               (company-tooltip-selection      :background ,bg+3)
               (company-tooltip-annotation     :foreground ,blue)
               (company-scrollbar-bg           :background ,bg+2 :height 0.3)
               (company-scrollbar-fg           :background ,bg+4 :height 0.3)
               (company-template-field         :inherit yas-field-highlight-face)

               (corfu-default                :background "#151321")
               (corfu-current                :background ,region)
               (corfu-border                :background ,bg+1)
               (tabnine-face                :forground ,white)

               ;; Yasnippet
               (yas-field-highlight-face       :background ,region2)

               ;; Meow
               (meow-keypad-indicator          :foreground "black" :background ,red)
               (meow-insert-indicator          :foreground "black" :background ,green)
               ;; `(meow-normal-indicator          ((t (:foreground "black" :background ,yellow))))
               (meow-normal-indicator          :foreground "black" :background ,yellow)
               (meow-motion-indicator          :foreground "black" :background ,blue)
               (meow-beacon-indicator          :foreground "black" :background ,purple)
               ;; (meow-keypad-cursor             ((t ())))
               (meow-insert-cursor             :background ,green)
               ;; (meow-normal-cursor             ((t ())))
               ;; (meow-motion-cursor             ((t ())))

               ;; Cider
               ;;
               (cider-result-overlay-face      :background "black")
               (cider-repl-stderr-face         :foreground ,blue)
               (cider-repl-stdout-face         :foreground ,fg-1)

               ;; Clojure
               ;;
               (clojure-character-face         :foreground ,purple)

               ;; Ivy
               ;; (ivy-highlight-face             ((t ())))
               (ivy-yanked-word                :background "yellow" :foreground "black")
               ;; (ivy-remote                     ((t ())))
               (ivy-current-match              :foreground ,bg :background ,bg)
               ;; (ivy-minibuffer-match-highlight ((t ())))
               ;; (ivy-minibuffer-match-face-1    ((t ())))
               ;; `(ivy-minibuffer-match-face-2    ((t ())))
               ;; `(ivy-minibuffer-match-face-3    ((t ())))
               ;; `(ivy-minibuffer-match-face-4    ((t ())))
               ;; (counsel-outline-default        ((t ())))
               (swiper-background-match-face-1  :inherit hl-line)
               (swiper-background-match-face-2  :inherit hl-line)
               (swiper-background-match-face-3 :inherit hl-line)
               (swiper-background-match-face-4  :inherit hl-line)
               (swiper-match-face-1           :foreground "white")
               (swiper-match-face-2            :foreground "white")
               (swiper-match-face-3            :foreground "white")
               (swiper-match-face-4            :foreground "white")

               ;; Selectrum
               (selectrum-current-candidate    :foreground ,bg :inverse-video t)

               ;; Magit
               (magit-diff-file-heading-highlight :background ,bg+1)
               (magit-section-highlight           :background ,bg+1)
               (magit-diff-removed             :inherit font-lock-string-face)
               (magit-diff-added               :inherit font-lock-comment-face)
               (magit-diff-removed-highlight   :inherit font-lock-string-face :background ,bg+2)
               (magit-diff-added-highlight     :inherit font-lock-comment-face :background ,bg+2)
               (magit-diff-highlight           :background ,bg+1)
               (magit-diff-context-highlight   :background ,bg+1)

               ;; SMerge
               (smerge-refined-added           :background "#253325")
               (smerge-lower                   :background "#173017")

               ;; Diff-hl
               (diff-hl-insert                 :foreground ,green :background ,green)
               (diff-hl-change                 :foreground ,blue :background ,blue)
               (diff-hl-delete                 :foreground ,red :background ,red)

               ;; Term
               (term-color-blue                :foreground ,blue :background ,blue)
               (term-color-green               :foreground ,green :background ,green)
               (term-color-red                 :foreground ,red :background ,red)

               ;; Popup
               (popup-tip-face                 :background ,bg+4 :foreground ,fg)
               (popup-isearch-match            :background ,brown :foreground "black")

               (tooltip                :background ,bg+4 :foreground ,fg)
               ;; `(tooltip ((t (:foreground ,black :background ,yellow))))
               (dired-directory                :foreground ,light-purple)
               ;; (web-mode-html-attr-name-face   ((t ())))
               ;; (web-mode-html-tag-face         ((t ())))

               ;; Emacs Rime
               (rime-preedit-face              :underline ,blue :background ,bg+2)
               (rime-cursor-face               :inherit font-lock-constant-face)
               (rime-indicator-face            :foreground ,purple)
               (rime-indicator-dim-face        :foreground ,bg+4)

               ;; Web Mode
               ;; (web-mode-function-call-face    ((t ())))
               ;; (web-mode-function-name-face    ((t ())))
               (web-mode-html-tag-bracket-face :inherit parenthesis)
               (web-mode-symbol-face           :foreground ,purple)
               (css-selector                   :foreground ,purple)

               ;; ;; Markdown
               ;; (markdown-header-face-1         :bold t :height ,(nth 0 my-dark-theme-header-scales))
               ;; (markdown-header-face-2         :bold t :height ,(nth 1 my-dark-theme-header-scales))
               ;; (markdown-header-face-3         :bold t :height ,(nth 2 my-dark-theme-header-scales))
               ;; (markdown-header-face-4         :bold t :height ,(nth 3 my-dark-theme-header-scales))
               ;; (markdown-header-face-5         :bold t :height ,(nth 4 my-dark-theme-header-scales))
               ;; (markdown-header-face-6         :bold t :height ,(nth 5 my-dark-theme-header-scales))
               ;; (markdown-header-face-7         :bold t :height ,(nth 6 my-dark-theme-header-scales))

               ;; Telega
               (telega-entity-type-code        :inherit font-lock-string-face)
               (telega-msg-heading             :inherit hl-line)
               (telega-unmuted-count           :inherit font-lock-function-name-face)

               ;; Org-mode
               (org-todo                      :foreground ,yellow)
               (org-done                      :foreground ,blue)
               (org-headline-todo             :foreground ,fg+1)
               (org-headline-done             :foreground ,fg-1 :strike-through t)
               (org-table                      :foreground ,fg+1)
               ;; (org-level-1                    :bold t :height ,(nth 0 my-dark-theme-header-scales))
               ;; (org-level-2                    :bold t :height ,(nth 1 my-dark-theme-header-scales))
               ;; (org-level-3                    :bold t :height ,(nth 2 my-dark-theme-header-scales))
               ;; (org-level-4                    :bold t :height ,(nth 3 my-dark-theme-header-scales))
               ;; (org-level-5                    :bold t :height ,(nth 4 my-dark-theme-header-scales))
               ;; (org-level-6                    :bold t :height ,(nth 5 my-dark-theme-header-scales))
               ;; (org-level-7                    :bold t :height ,(nth 6 my-dark-theme-header-scales))
               (org-document-title             inherit font-lock-string-face)
               (org-code                       inherit font-lock-constant-face)

               (mu4e-unread-face                            :inherit font-lock-keyword-face :weight bold)
               (mu4e-trashed-face                            :inherit font-lock-comment-face :strike-through t)
               (mu4e-draft-face                              :inherit font-lock-string-face)
               (mu4e-flagged-face                            :inherit font-lock-constant-face :weight bold)
               (mu4e-replied-face                          :inherit font-lock-builtin-face :weight normal :slant normal)
               (mu4e-forwarded-face                          :inherit font-lock-builtin-face :weight normal :slant normal)
               (mu4e-header-face                               :inherit default)
               (mu4e-related-face                              :inherit default :slant ,poly-dark-theme-use-italic)
               (mu4e-header-title-face                      :inherit font-lock-type-face)
               ;; (mu4e-header-highlight-face                  :inherit hl-line :weight bold :underline t
               ;;               ,@(and (>= emacs-major-version 27) '(:extend t)
               (mu4e-header-marks-face                       :inherit font-lock-preprocessor-face)
               (mu4e-header-key-face                        :inherit message-header-name :weight bold)
               (mu4e-header-value-face                         :inherit font-lock-type-face)
               (mu4e-special-header-value-face                  :inherit font-lock-builtin-face)
               (mu4e-link-face                             :inherit link)
               (mu4e-contact-face                          :inherit font-lock-variable-name-face)
               (mu4e-highlight-face                         :inherit highlight)
               (mu4e-title-face                             :inherit font-lock-type-face :weight bold)
               (mu4e-modeline-face                           :inherit font-lock-string-face :weight bold)
               (mu4e-footer-face                             :inherit font-lock-comment-face)
               (mu4e-url-number-face                         :inherit font-lock-constant-face :weight bold)
               (mu4e-system-face                             :inherit font-lock-comment-face :slant ,poly-dark-theme-use-italic)
               (mu4e-ok-face                                 :inherit font-lock-comment-face :weight bold :slant normal)
               (mu4e-warning-face                            :inherit font-lock-warning-face :weight bold :slant normal)
               (mu4e-compose-separator-face                  :inherit message-separator :slant ,poly-dark-theme-use-italic)
               (mu4e-region-code                             :background "DarkSlateGray")
               (mu4e-attach-number-face                     :inherit font-lock-variable-name-face :weight bold)
               (mu4e-cited-1-face                            :inherit font-lock-builtin-face :weight normal :slant ,poly-dark-theme-use-italic)
               (mu4e-cited-2-face                            :inherit font-lock-preprocessor-face :weight normal :slant ,poly-dark-theme-use-italic)
               (mu4e-cited-3-face                            :inherit font-lock-variable-name-face :weight normal :slant ,poly-dark-theme-use-italic)
               (mu4e-cited-4-face                            :inherit font-lock-keyword-face :weight normal :slant ,poly-dark-theme-use-italic)
               (mu4e-cited-5-face                            :inherit font-lock-comment-face :weight normal :slant poly-dark-theme-use-italic)
               (mu4e-cited-6-face                            :inherit font-lock-comment-delimiter-face :weight normal :slant ,poly-dark-theme-use-italic)
               (mu4e-cited-7-face                            :inherit font-lock-type-face :weight normal :slant ,poly-dark-theme-use-italic)
               (mu4e-compose-header-face                     :inherit message-separator :slant ,poly-dark-theme-use-italic)
               (mu4e-context-face                            :inherit mu4e-title-face :weight bold)
               ;; `(mu4e-moved-face                           ())))
               (mu4e-view-body-face                          :inherit default)
               (mu4e-modeline-face                           :inherit font-lock-string-face :weight bold)

               ;; Treemacs
               (treemacs-root-face             :inherit font-lock-function-name-face :height 1.4 :underline t)

               (fill-column-indicator          :foreground ,bg+2)

               (scroll-bar                     :foreground ,fg-1)

               (tab-bar                        :background ,bg+2)
               (tab-bar-tab                    :inverse-video t :bold t :background ,blue :foreground ,bg+2)
               (tab-bar-tab-inactive           :inherit shadow)
               (tab-bar-tab-ungrouped :inherit ,bg+2)
	       ;; (tab-bar-tab-group-current      ())))
	       ;; (tab-bar-tab-group-inactive     ())
	       (icomplete-first-match :inherit mode-line-emphasis)
	       (mode-line-buffer-id :foreground "Light Blue")
	       (font-lock-variable-name-face :foreground "#50fa7b")
	       ;; (highlight-indentation-face (:inherit default :foreground "#878787"))))
	       ;; (hl-line :background ,bg+1)
	       (orderless-match-face-0 :inherit font-lock-type-face :weight bold)
	       (orderless-match-face-1 :inherit error :weight bold)
	       (orderless-match-face-2 :inherit font-lock-string-face :weight bold)
	       (orderless-match-face-3 :inherit font-lock-keyword-face :weight bold)
	       (font-lock-variable-name-face :foreground "#50fa7b")
	       (highlight-indentation-face :inherit default :foreground "#878787")
	       (hl-line :background "DodgerBlue4")
	       (mode-line :foreground ,purple :background ,red)
	       ;; `(mode-line-inactive (:family "Noto Sans" :height 100))))

	       ;; auto-highlight-symbol
	       (ahs-definition-face :foreground ,magenta :background unspecified :slant normal)
	       (ahs-definition-face-unfocused :foreground ,magenta :background unspecified :slant normal)
	       (ahs-edit-mode-face :foreground ,bg+1 :background ,magenta)
	       (ahs-face :foreground ,magenta :background unspecified)
	       (ahs-face-unfocused :foreground ,magenta :background unspecified)
	       (ahs-plugin-bod-face :foreground ,magenta :background unspecified)
	       (ahs-plugin-default-face :foreground ,magenta :background unspecified)
	       (ahs-plugin-default-face-unfocused :foreground ,magenta :background unspecified)
	       (ahs-plugin-whole-buffer-face :foreground ,magenta  :background unspecified)
	       (ahs-warning-face :foreground ,red :weight bold)
	       )))

  (apply #'custom-theme-set-faces
         'poly-dark
         (let* ((color-names (mapcar #'car colors))
		(graphic-colors (mapcar #'cadr colors))
		(term-colors (mapcar #'car (mapcar #'cddr colors)))
		(tty-colors (mapcar #'car (mapcar #'last colors)))
		(expand-for-kind (lambda (kind spec)
        			   (cl-progv color-names kind
        			     (eval `(backquote ,spec))))))
           (cl-loop for (face . spec) in faces
        	    collect `(,face
        		      ((((min-colors 16777216)) ; fully graphical envs
        			,(funcall expand-for-kind graphic-colors spec))
        		       (((min-colors 256))      ; terminal withs 256 colors
        			,(funcall expand-for-kind term-colors spec))
        		       (t                       ; should be only tty-like envs
        			,(funcall expand-for-kind tty-colors spec))))))

	 ))


;; Adjust default font height when running in HiDPI screen.
(when (> (frame-pixel-width) 3000)
  (custom-set-faces '(corfu-default ((t (:height 1.3))))))

;; (set-face-attribute 'mode-line nil :inherit 'mode-line)
;; (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'poly-dark)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; poly-dark-theme.el ends here
