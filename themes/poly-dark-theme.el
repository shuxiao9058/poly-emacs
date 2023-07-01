;;; -*- mode: emacs-lisp; eval: (rainbow-mode); lexical-binding: t -*-

;; Package-Requires: ((emacs "24.3"))

;; list-faces-display

;;; Code:

(require 'cl-lib)

(deftheme poly-dark
  "My dark poly theme.")

;;;; Configuration options:

(defgroup poly-dark nil
  "Poly dark theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defvar poly-dark-use-italic t
  "Non-nil means use italic for comment and docstring.")

(defcustom poly-dark-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'poly-dark)

(defvar poly-dark-header-scales
  '(1.6 1.4 1.2 1.2 1.2 1.1 1.0)
  "Scales for headers.")

(defcustom poly-dark-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'poly-dark)

(defcustom poly-dark-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'poly-dark)

(defcustom poly-dark-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'poly-dark)

(defcustom poly-dark-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'poly-dark)

;; (defcustom poly-dark-height-doc-title 1.44
;;   "Font size 144%."
;;   :type 'number
;;   :group 'poly-dark)

(defcustom poly-dark-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'poly-dark)

(defface tabnine-face '((t (:inherit font-lock-string-face)))
  "TabNine face.")

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
                (dogblue4 "#104e8b" "#104e8b" "DodgerBlue4") ;; DodgerBlue4

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
		(light-purple "#B28CE2" "brightblue")
		(brown "#CFA300" "#F9C986" "brightred")
		(region "#454158" "#3F3D5C" "brightblack")
		(region2 "#350035" "#3F3D5C" "brightblack")
		(magenta "#ff05fa" "#F986BF" "magenta")))

      (faces '( ;; default
               (button                         :foreground ,blue :underline t)
               (window-divider                 :background ,bg+3)
               (window-divider-first-pixel     :foreground ,bg+1)
               (window-divider-last-pixel      :foreground ,bg+1)
               (line-number-current-line       :foreground ,yellow)

               (parenthesis                    :foreground ,fg-1)
               ;; ;; (completions-common-part        )
               (xref-match                     :inherit match)
               (secondary-selection           :background ,region2 :extend t)

               (compilation-info               :inherit font-lock-function-name-face)
               (compilation-warning            :inherit font-lock-warning-face)

               ;; ;; IMenu
               (imenu-list-entry-subalist-face-0  :bold t)

               ;; Yascroll
               (yascroll:thumb-fringe          :background ,fg :foreground ,fg)
               (yascroll:thumb-text-area       :background ,fg :foreground ,fg)

               (corfu-default                :background "#151321")
               (corfu-current                :background ,region)
               (corfu-border                :background ,bg+1)
               (tabnine-face                :forground ,white)

               ;; Yasnippet
               (yas-field-highlight-face       :background ,region2)

               ;; Cider
               (cider-result-overlay-face      :background "black")
               (cider-repl-stderr-face         :foreground ,blue)
               (cider-repl-stdout-face         :foreground ,fg-1)

               ;; Clojure
               (clojure-character-face         :foreground ,purple)

               ;; ;; Ivy
               (ivy-highlight-face            :inherit highlight)
               (ivy-yanked-word               :inherit highlight)
               (ivy-remote                    :foreground ,pink)
               ;; (ivy-current-match :foreground ,orange :inverse-video t)
               (ivy-current-match
	        ,@(if poly-dark-alternate-mode-line-and-minibuffer
	              (list :weight 'normal :foreground green)
	            (list :weight 'bold :foreground pink)))
               (ivy-minibuffer-match-highlight :foreground ,orange)
               (ivy-minibuffer-match-face-1   :background ,dogblue4 :foreground ,bg)
               (ivy-minibuffer-match-face-2   :background ,green :foreground ,bg)
               (ivy-minibuffer-match-face-3   :background ,yellow :foreground ,bg)
               (ivy-minibuffer-match-face-4   :background ,pink :foreground ,bg)
               (ivy-virtual :foreground ,cyan)
               (ivy-subdir :foreground ,yellow)
               (ivy-confirm-face :foreground ,orange)
               (ivy-match-required-face :foreground ,red)
               ;; ;; (counsel-outline-default        ((t ())))
               (swiper-background-match-face-1  :inherit hl-line)
               (swiper-background-match-face-2  :inherit hl-line)
               (swiper-background-match-face-3 :inherit hl-line)
               (swiper-background-match-face-4  :inherit hl-line)
               (swiper-match-face-1           :foreground "white")
               (swiper-match-face-2            :foreground "white")
               (swiper-match-face-3            :foreground "white")
               (swiper-match-face-4            :foreground "white")

               ;; Selectrum
               ;; (selectrum-current-candidate    :foreground ,bg :inverse-video t)
               (selectrum-current-candidate    :foreground ,orange :inverse-video t)
               (vertico-current                :foreground ,orange :inverse-video t)

               ;; SMerge
               (smerge-refined-added           :background "#253325")
               (smerge-lower                   :background "#173017")

               ;; Popup
               (popup-tip-face                 :background ,bg+4 :foreground ,fg)
               (popup-isearch-match            :background ,brown :foreground "black")

               (tooltip                :background ,bg+4 :foreground ,fg)
               ;; ;; `(tooltip ((t (:foreground ,black :background ,yellow))))

               ;; Emacs Rime
               (rime-preedit-face              :underline ,blue :background ,bg+2)
               (rime-cursor-face               :inherit font-lock-constant-face)
               (rime-indicator-face            :foreground ,purple)
               (rime-indicator-dim-face        :foreground ,bg+4)

               ;; ;; Web Mode
               ;; ;; (web-mode-function-call-face    ((t ())))
               ;; ;; (web-mode-function-name-face    ((t ())))
               (web-mode-html-tag-bracket-face :inherit parenthesis)
               (web-mode-symbol-face           :foreground ,purple)
               (css-selector                   :foreground ,purple)

               ;; Telega
               (telega-entity-type-code        :inherit font-lock-string-face)
               (telega-msg-heading             :inherit hl-line)
               (telega-unmuted-count           :inherit font-lock-function-name-face)

               ;; ;; Org-mode
               (org-headline-todo             :foreground ,fg+1)
               ;; ;; (org-level-1                    :bold t :height ,(nth 0 poly-dark-header-scales))
               ;; ;; (org-level-2                    :bold t :height ,(nth 1 poly-dark-header-scales))
               ;; ;; (org-level-3                    :bold t :height ,(nth 2 poly-dark-header-scales))
               ;; ;; (org-level-4                    :bold t :height ,(nth 3 poly-dark-header-scales))
               ;; ;; (org-level-5                    :bold t :height ,(nth 4 poly-dark-header-scales))
               ;; ;; (org-level-6                    :bold t :height ,(nth 5 poly-dark-header-scales))
               ;; ;; (org-level-7                    :bold t :height ,(nth 6 poly-dark-header-scales))

               (mu4e-trashed-face                            :inherit font-lock-comment-face :strike-through t)
               (mu4e-draft-face                              :inherit font-lock-string-face)
               (mu4e-flagged-face                            :inherit font-lock-constant-face :weight bold)
               (mu4e-replied-face                          :inherit font-lock-builtin-face :weight normal :slant normal)
               (mu4e-forwarded-face                          :inherit font-lock-builtin-face :weight normal :slant normal)
               (mu4e-header-face                               :inherit default)
               (mu4e-related-face                              :inherit default :slant ,poly-dark-use-italic)
               (mu4e-header-title-face                      :inherit font-lock-type-face)
               (mu4e-header-highlight-face                  :inherit hl-line :weight bold :underline t
							    ,@(and (>= emacs-major-version 27) '(:extend t)))
	       (mu4e-header-value-face                         :inherit font-lock-type-face)
	       (mu4e-special-header-value-face                  :inherit font-lock-builtin-face)
	       (mu4e-link-face                             :inherit link)
	       (mu4e-contact-face                          :inherit font-lock-variable-name-face)
	       (mu4e-title-face                             :inherit font-lock-type-face :weight bold)
	       (mu4e-modeline-face                           :inherit font-lock-string-face :weight bold)
	       (mu4e-footer-face                             :inherit font-lock-comment-face)
	       (mu4e-url-number-face                         :inherit font-lock-constant-face :weight bold)
	       (mu4e-system-face                             :inherit font-lock-comment-face :slant ,poly-dark-use-italic)
	       (mu4e-ok-face                                 :inherit font-lock-comment-face :weight bold :slant normal)
	       (mu4e-warning-face                            :inherit font-lock-warning-face :weight bold :slant normal)
	       (mu4e-compose-separator-face                  :inherit message-separator :slant ,poly-dark-use-italic)
	       (mu4e-region-code                             :background "DarkSlateGray")
	       (mu4e-attach-number-face                     :inherit font-lock-variable-name-face :weight bold)
	       (mu4e-compose-header-face                     :inherit message-separator :slant ,poly-dark-use-italic)
	       (mu4e-context-face                            :inherit mu4e-title-face :weight bold)
	       ;; ;; `(mu4e-moved-face                           ())))
	       (mu4e-view-body-face                          :inherit default)

	       ;; Treemacs
	       (treemacs-root-face             :inherit font-lock-function-name-face :height 1.4 :underline t)

	       (fill-column-indicator          :foreground ,bg+2 :height 0.10)

	       (scroll-bar                     :foreground ,fg-1)


	       (icomplete-first-match :inherit mode-line-emphasis)
	       (orderless-match-face-0 :inherit font-lock-type-face :weight bold)
	       (orderless-match-face-1 :inherit error :weight bold)
	       (orderless-match-face-2 :inherit font-lock-string-face :weight bold)
	       (orderless-match-face-3 :inherit font-lock-keyword-face :weight bold)

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

	       (cursor :background ,fg3)
	       (completions-first-difference :foreground ,pink :weight bold)
	       (default :background ,bg :foreground ,fg)
	       (default-italic :slant italic)
	       (ffap :foreground ,fg4)
	       (fringe :background ,bg :foreground ,fg4)
	       (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,dogblue4 :extend t)
	       (info-quoted-name :foreground ,orange)
	       (info-string :foreground ,yellow)
	       (lazy-highlight :foreground ,fg2 :background ,bg2)
	       (link :foreground ,cyan :underline t)
	       (linum :slant italic :foreground ,bg4 :background ,bg)
	       (line-number :slant italic :foreground ,bg4 :background ,bg)
	       (match :background ,yellow :foreground ,bg)
	       (minibuffer-prompt
		,@(if poly-dark-alternate-mode-line-and-minibuffer
		      (list :weight 'normal :foreground fg)
		    (list :weight 'bold :foreground pink)))
	       (read-multiple-choice-face :inherit completions-first-difference)
	       (region :inherit match :extend t)
	       (trailing-whitespace :foreground nil :background ,orange)
	       (vertical-border :foreground ,bg2)
	       (success :foreground ,green)
	       (warning :foreground ,orange)
	       (error :foreground ,red)

               (flymake-errline :underline ,red :foreground ,red :background nil :inherit nil)
               (flymake-warnline :underline ,orange :foreground ,orange :background nil :inherit nil)

	       (header-line :background ,bg)
	       ;; syntax
	       (font-lock-builtin-face :foreground ,orange)
	       (font-lock-comment-face :foreground ,comment :italic t)
	       (font-lock-comment-delimiter-face :foreground ,comment :italic t)
	       (font-lock-constant-face :foreground ,cyan)
	       (font-lock-doc-face :foreground ,comment)
	       (font-lock-function-name-face :foreground ,green :weight bold)
	       (font-lock-keyword-face :weight bold :foreground ,pink)
	       (font-lock-negation-char-face :foreground ,cyan)
	       (font-lock-preprocessor-face :foreground ,orange)
	       (font-lock-reference-face :foreground ,cyan)
	       (font-lock-regexp-grouping-backslash :foreground ,cyan)
	       (font-lock-regexp-grouping-construct :foreground ,purple)
	       (font-lock-string-face :foreground ,yellow)
	       (font-lock-type-face :foreground ,purple)
	       (font-lock-variable-name-face :foreground ,fg
					     ;; :weight bold
                                             )
	       (font-lock-warning-face :foreground ,orange :background ,bg2)
	       (font-lock-operator-face   :foreground ,red)
	       (font-lock-number-face         :foreground ,purple)
	       (font-lock-delimiter-face         :foreground ,white)
	       (font-lock-bracket-face         :foreground ,red)
	       (font-lock-escape-face         :foreground ,red)
	       (font-lock-property-face         :foreground ,red)

	       ;; auto-complete
	       (ac-completion-face :underline t :foreground ,pink)
	       ;; company
	       (company-echo-common :foreground ,bg :background ,fg)
	       (company-preview :background ,bg :foreground ,alt-blue)
	       (company-preview-common :foreground ,bg2 :foreground ,fg3)
	       (company-preview-search :foreground ,purple :background ,bg)
	       (company-scrollbar-bg :background ,bg3)
	       (company-scrollbar-fg :foreground ,pink)
	       (company-template-field :inherit match)
	       (company-tooltip :foreground ,fg2 :background ,bg :weight bold)
	       (company-tooltip-annotation :foreground ,cyan)
	       (company-tooltip-common :foreground ,fg3)
	       (company-tooltip-common-selection :foreground ,yellow)
	       (company-tooltip-mouse :inherit highlight)
	       (company-tooltip-selection :background ,bg3 :foreground ,fg3)

               ;; (custom-changed :foreground "#8Fd4FF" :background "#DDDDFF"
	       ;;  	       :extend t)
               (custom-changed :foreground "#F8F8F8" :background "#4A410D"
			       :extend t)
               (diff-added-face :background "#335533"
				:foreground "#ddffdd"
				:extend t)
	       (diff-changed :foreground "#F8F8F8" :background "#4A410D"
                             :extend t)
	       (diff-removed-face :background "#553333"
				  :foreground "#ffdddd"
				  :extend t)
	       (diff-context-face :foreground ,fg3)
	       (diff-file-header-face :foreground "gold" :background unspecified)
	       (diff-function-face :foreground "gray50" :background unspecified)
	       (diff-header-face :foreground "gold" :background unspecified)
	       (diff-hunk-header-face :foreground "gold")
	       (diff-index-face :bold t :background "gray70")
	       (diff-nonexistent-face :bold t :background "gray70")

	       ;; diff-hl
	       (diff-hl-change :foreground ,orange :background ,orange)
	       (diff-hl-delete :foreground ,red :background ,red)
	       (diff-hl-insert :foreground ,green :background ,green)

	       ;; dired
	       (dired-directory :foreground ,green :weight normal)
	       (dired-flagged :foreground ,pink)
	       (dired-header :foreground ,fg3 :background ,bg)
	       (dired-ignored :inherit shadow)
	       (dired-mark :foreground ,fg :weight bold)
	       (dired-marked :foreground ,orange :weight bold)
	       (dired-perm-write :foreground ,fg3 :underline t)
	       (dired-symlink :foreground ,yellow :weight normal :slant italic)
	       (dired-warning :foreground ,orange :underline t)
	       (diredp-compressed-file-name :foreground ,fg3)
	       (diredp-compressed-file-suffix :foreground ,fg4)
	       (diredp-date-time :foreground ,fg)
	       (diredp-deletion-file-name :foreground ,pink :background ,current)
	       (diredp-deletion :foreground ,pink :weight bold)
	       (diredp-dir-heading :foreground ,fg2 :background ,bg4)
	       (diredp-dir-name :inherit dired-directory)
	       (diredp-dir-priv :inherit dired-directory)
	       (diredp-executable-tag :foreground ,orange)
	       (diredp-file-name :foreground ,fg)
	       (diredp-file-suffix :foreground ,fg4)
	       (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,current)
	       (diredp-flag-mark :foreground ,fg2 :weight bold :background ,current)
	       (diredp-ignored-file-name :foreground ,fg)
	       (diredp-mode-line-flagged :foreground ,orange)
	       (diredp-mode-line-marked :foreground ,orange)
	       (diredp-no-priv :foreground ,fg)
	       (diredp-number :foreground ,cyan)
	       (diredp-other-priv :foreground ,orange)
	       (diredp-rare-priv :foreground ,orange)
	       (diredp-read-priv :foreground ,purple)
	       (diredp-write-priv :foreground ,pink)
	       (diredp-exec-priv :foreground ,yellow)
	       (diredp-symlink :foreground ,orange)
	       (diredp-link-priv :foreground ,orange)
	       (diredp-autofile-name :foreground ,yellow)
	       (diredp-tagged-autofile-name :foreground ,yellow)
	       ;; enh-ruby
	       (enh-ruby-heredoc-delimiter-face :foreground ,yellow)
	       (enh-ruby-op-face :foreground ,pink)
	       (enh-ruby-regexp-delimiter-face :foreground ,yellow)
	       (enh-ruby-string-delimiter-face :foreground ,yellow)
	       ;; flyspell
	       (flyspell-duplicate :underline (:style wave :color ,orange))
	       (flyspell-incorrect :underline (:style wave :color ,red))
	       ;; font-latex
	       (font-latex-bold-face :foreground ,purple)
	       (font-latex-italic-face :foreground ,pink :slant italic)
	       (font-latex-match-reference-keywords :foreground ,cyan)
	       (font-latex-match-variable-keywords :foreground ,fg)
	       (font-latex-string-face :foreground ,yellow)
	       ;; gnus-group
	       (gnus-group-mail-1 :foreground ,pink :weight bold)
	       (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
	       (gnus-group-mail-2 :foreground ,cyan :weight bold)
	       (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
	       (gnus-group-mail-3 :foreground ,comment :weight bold)
	       (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
	       (gnus-group-mail-low :foreground ,current :weight bold)
	       (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
	       (gnus-group-news-1 :foreground ,pink :weight bold)
	       (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
	       (gnus-group-news-2 :foreground ,cyan :weight bold)
	       (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
	       (gnus-group-news-3 :foreground ,comment :weight bold)
	       (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
	       (gnus-group-news-4 :inherit gnus-group-news-low)
	       (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
	       (gnus-group-news-5 :inherit gnus-group-news-low)
	       (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
	       (gnus-group-news-6 :inherit gnus-group-news-low)
	       (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
	       (gnus-group-news-low :foreground ,current :weight bold)
	       (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
	       (gnus-header-content :foreground ,pink)
	       (gnus-header-from :foreground ,fg)
	       (gnus-header-name :foreground ,purple)
	       (gnus-header-subject :foreground ,green :weight bold)
	       (gnus-summary-markup-face :foreground ,cyan)
	       (gnus-summary-high-unread :foreground ,pink :weight bold)
	       (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
	       (gnus-summary-high-ancient :inherit gnus-summary-high-read)
	       (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
	       (gnus-summary-normal-unread :foreground ,alt-blue :weight bold)
	       (gnus-summary-normal-read :foreground ,comment :weight normal)
	       (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
	       (gnus-summary-normal-ticked :foreground ,pink :weight bold)
	       (gnus-summary-low-unread :foreground ,comment :weight bold)
	       (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
	       (gnus-summary-low-ancient :inherit gnus-summary-low-read)
	       (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
	       (gnus-summary-selected :inverse-video t)
	       ;; haskell-mode
	       (haskell-operator-face :foreground ,pink)
	       (haskell-constructor-face :foreground ,purple)
	       ;; helm
	       (helm-bookmark-w3m :foreground ,purple)
	       (helm-buffer-not-saved :foreground ,purple :background ,bg)
	       (helm-buffer-process :foreground ,orange :background ,bg)
	       (helm-buffer-saved-out :foreground ,fg :background ,bg)
	       (helm-buffer-size :foreground ,fg :background ,bg)
	       (helm-candidate-number :foreground ,bg :background ,fg)
	       (helm-ff-directory :foreground ,green :background ,bg :weight bold)
	       (helm-ff-dotted-directory :foreground ,green :background ,bg :weight normal)
	       (helm-ff-executable :foreground ,alt-blue :background ,bg :weight normal)
	       (helm-ff-file :foreground ,fg :background ,bg :weight normal)
	       (helm-ff-invalid-symlink :foreground ,pink :background ,bg :weight bold)
	       (helm-ff-prefix :foreground ,bg :background ,pink :weight normal)
	       (helm-ff-symlink :foreground ,pink :background ,bg :weight bold)
	       (helm-grep-cmd-line :foreground ,fg :background ,bg)
	       (helm-grep-file :foreground ,fg :background ,bg)
	       (helm-grep-finish :foreground ,fg2 :background ,bg)
	       (helm-grep-lineno :foreground ,fg :background ,bg)
	       (helm-grep-match :foreground nil :background nil :inherit helm-match)
	       (helm-grep-running :foreground ,green :background ,bg)
	       (helm-header :foreground ,fg2 :background ,bg :underline nil :box nil)
	       (helm-moccur-buffer :foreground ,green :background ,bg)
	       (helm-selection :background ,bg2 :underline nil)
	       (helm-selection-line :background ,bg2)
	       (helm-separator :foreground ,purple :background ,bg)
	       (helm-source-go-package-godoc-description :foreground ,yellow)
	       (helm-source-header :foreground ,pink :background ,bg :underline nil :weight bold)
	       (helm-time-zone-current :foreground ,orange :background ,bg)
	       (helm-time-zone-home :foreground ,purple :background ,bg)
	       (helm-visible-mark :foreground ,bg :background ,bg3)
	       ;; highlight-indentation minor mode
	       (highlight-indentation-face :background ,bg2)

	       (icicle-whitespace-highlight :background ,fg)
	       (icicle-special-candidate :foreground ,fg2)
	       (icicle-extra-candidate :foreground ,fg2)
	       (icicle-search-main-regexp-others :foreground ,fg)
	       (icicle-search-current-input :foreground ,pink)
	       (icicle-search-context-level-8 :foreground ,orange)
	       (icicle-search-context-level-7 :foreground ,orange)
	       (icicle-search-context-level-6 :foreground ,orange)
	       (icicle-search-context-level-5 :foreground ,orange)
	       (icicle-search-context-level-4 :foreground ,orange)
	       (icicle-search-context-level-3 :foreground ,orange)
	       (icicle-search-context-level-2 :foreground ,orange)
	       (icicle-search-context-level-1 :foreground ,orange)
	       (icicle-search-main-regexp-current :foreground ,fg)
	       (icicle-saved-candidate :foreground ,fg)
	       (icicle-proxy-candidate :foreground ,fg)
	       (icicle-mustmatch-completion :foreground ,purple)
	       (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
	       (icicle-msg-emphasis :foreground ,green)
	       (icicle-mode-line-help :foreground ,fg4)
	       (icicle-match-highlight-minibuffer :foreground ,orange)
	       (icicle-match-highlight-Completions :foreground ,green)
	       (icicle-key-complete-menu-local :foreground ,fg)
	       (icicle-key-complete-menu :foreground ,fg)
	       (icicle-input-completion-fail-lax :foreground ,pink)
	       (icicle-input-completion-fail :foreground ,pink)
	       (icicle-historical-candidate-other :foreground ,fg)
	       (icicle-historical-candidate :foreground ,fg)
	       (icicle-current-candidate-highlight :foreground ,orange :background ,bg3)
	       (icicle-Completions-instruction-2 :foreground ,fg4)
	       (icicle-Completions-instruction-1 :foreground ,fg4)
	       (icicle-completion :foreground ,fg)
	       (icicle-complete-input :foreground ,orange)
	       (icicle-common-match-highlight-Completions :foreground ,purple)
	       (icicle-candidate-part :foreground ,fg)
	       (icicle-annotation :foreground ,fg4)
	       ;; icomplete
	       (icompletep-determined :foreground ,orange)
	       ;; ido
	       (ido-first-match
		,@(if poly-dark-alternate-mode-line-and-minibuffer
		      (list :weight 'normal :foreground green)
		    (list :weight 'bold :foreground pink)))
	       (ido-only-match :foreground ,orange)
	       (ido-subdir :foreground ,yellow)
	       (ido-virtual :foreground ,cyan)
	       (ido-incomplete-regexp :inherit font-lock-warning-face)
	       (ido-indicator :foreground ,fg :background ,pink)
	       ;; isearch
               ;; ;; ISearch
	       (isearch :inherit match :weight bold)
	       (isearch-fail :foreground ,bg :background ,orange)
	       ;; jde-java
	       (jde-java-font-lock-constant-face :foreground ,cyan)
	       (jde-java-font-lock-modifier-face :foreground ,pink)
	       (jde-java-font-lock-number-face :foreground ,fg)
	       (jde-java-font-lock-package-face :foreground ,fg)
	       (jde-java-font-lock-private-face :foreground ,pink)
	       (jde-java-font-lock-public-face :foreground ,pink)
	       ;; js2-mode
	       (js2-external-variable :foreground ,purple)
	       (js2-function-param :foreground ,cyan)
	       (js2-jsdoc-html-tag-delimiter :foreground ,yellow)
	       (js2-jsdoc-html-tag-name :foreground ,alt-blue)
	       (js2-jsdoc-value :foreground ,yellow)
	       (js2-private-function-call :foreground ,cyan)
	       (js2-private-member :foreground ,fg3)
	       ;; js3-mode
	       (js3-error-face :underline ,orange)
	       (js3-external-variable-face :foreground ,fg)
	       (js3-function-param-face :foreground ,pink)
	       (js3-instance-member-face :foreground ,cyan)
	       (js3-jsdoc-tag-face :foreground ,pink)
	       (js3-warning-face :underline ,pink)
	       ;; magit
	       (magit-branch-local :foreground ,cyan)
	       (magit-branch-remote :foreground ,green)
	       (magit-tag :foreground ,orange)
	       (magit-section-heading :foreground ,pink :weight bold)
	       (magit-section-highlight :background ,bg3 :extend t)
	       (magit-diff-context-highlight :background ,bg3
					     :foreground ,fg3
					     :extend t)
	       (magit-diff-highlight           :background ,bg+1)
	       (magit-diff-revision-summary :foreground ,orange
					    :background ,bg
					    :weight bold)
	       (magit-diff-revision-summary-highlight :foreground ,orange
						      :background ,bg3
						      :weight bold
						      :extend t)
	       ;; the four following lines are just a patch of the
	       ;; upstream color to add the extend keyword.
	       (magit-diff-added :background "#335533"
				 :foreground "#ddffdd"
				 :extend t)
	       (magit-diff-added-highlight :background "#336633"
					   :foreground "#cceecc"
					   :extend t)
	       (magit-diff-removed :background "#553333"
				   :foreground "#ffdddd"
				   :extend t)
	       (magit-diff-removed-highlight :background "#663333"
					     :foreground "#eecccc"
					     :extend t)
	       (magit-diff-file-heading :foreground ,fg)
	       (magit-diff-file-heading-highlight :inherit magit-section-highlight)
	       (magit-diffstat-added :foreground ,green)
	       (magit-diffstat-removed :foreground ,red)
	       (magit-hash :foreground ,fg2)
	       (magit-hunk-heading :background ,bg3)
	       (magit-hunk-heading-highlight :background ,bg3)
	       (magit-item-highlight :background ,bg3)
	       (magit-log-author :foreground ,fg3)
	       (magit-process-ng :foreground ,orange :weight bold)
	       (magit-process-ok :foreground ,green :weight bold)

               (ediff-current-diff-face-A :background ,green :foreground "firebrick")
	       (ediff-current-diff-face-Ancestor :background "VioletRed" :foreground "Black")
	       (ediff-current-diff-face-B :background "Yellow" :foreground "DarkOrchid")
	       (ediff-current-diff-face-C :background "Pink" :foreground "Navy")
	       (ediff-even-diff-face-A :background "light grey" :foreground "Black")
	       (ediff-even-diff-face-Ancestor :background "Grey" :foreground "White")
	       (ediff-even-diff-face-B :background "Grey" :foreground "White")
	       (ediff-even-diff-face-C :background "light grey" :foreground "Black")
	       (ediff-fine-diff-face-A :background "sky blue" :foreground "Navy")
	       (ediff-fine-diff-face-Ancestor :background "Green" :foreground "Black")
	       (ediff-fine-diff-face-B :background "cyan" :foreground "Black")
	       (ediff-fine-diff-face-C :background "Turquoise" :foreground "Black")
	       (ediff-odd-diff-face-A :background "Grey" :foreground "White")
	       (ediff-odd-diff-face-Ancestor :background "light grey" :foreground "Black")
	       (ediff-odd-diff-face-B :background "light grey" :foreground "Black")
	       (ediff-odd-diff-face-C :background "Grey" :foreground "White")

               (smerge-base-face :foreground ,red)
	       (smerge-markers-face :background ,fg3)
	       (smerge-mine-face :foreground ,blue)
	       (smerge-other-face :foreground "DarkOliveGreen4")

               (log-view-file-face :bold t :background ,fg3 :weight bold)
	       (log-view-message-face :foreground ,orange)

	       ;; markdown
	       (markdown-blockquote-face :foreground ,orange)
	       (markdown-code-face :foreground ,orange)
	       (markdown-footnote-face :foreground ,alt-blue)
	       (markdown-header-face :weight normal)
	       (markdown-header-face-1
		:inherit bold :foreground ,pink
		,@(when poly-dark-enlarge-headings
		    (list :height poly-dark-height-title-1)))
	       (markdown-header-face-2
		:inherit bold :foreground ,purple
		,@(when poly-dark-enlarge-headings
		    (list :height poly-dark-height-title-2)))
	       (markdown-header-face-3
		:foreground ,green
		,@(when poly-dark-enlarge-headings
		    (list :height poly-dark-height-title-3)))
	       (markdown-header-face-4 :foreground ,yellow)
	       (markdown-header-face-5 :foreground ,cyan)
	       (markdown-header-face-6 :foreground ,orange)
	       (markdown-header-face-7 :foreground ,alt-blue)
	       (markdown-header-face-8 :foreground ,fg)
	       (markdown-inline-code-face :foreground ,yellow)
	       (markdown-plain-url-face :inherit link)
	       (markdown-pre-face :foreground ,orange)
	       (markdown-table-face :foreground ,purple)
	       ;; message
	       (message-mml :foreground ,green :weight normal)
	       (message-header-xheader :foreground ,cyan :weight normal)
	       ;; mode-line
	       (mode-line :background ,current
			  :box ,current :inverse-video nil
			  ,@(if poly-dark-alternate-mode-line-and-minibuffer
				(list :foreground fg3)
			      (list :foreground nil)))
	       (mode-line-inactive
		:inverse-video nil
		,@(if poly-dark-alternate-mode-line-and-minibuffer
		      (list :foreground comment :background bg
			    :box bg)
		    (list :foreground fg :background bg2 :box bg2)))
               ;;;; mode-line / header-line
	       (mode-line-active    :inherit mode-line)
	       (mode-line-emphasis  ::inherit highlight :distant-foreground ,bg)
	       (mode-line-highlight :inherit highlight :distant-foreground ,bg)
               (mode-line-buffer-id :foreground "Light Blue" :weight bold)
	       ;; (mode-line-buffer-id :weight 'bold)
	       (header-line :inherit mode-line)
	       (header-line-highlight :inherit mode-line-highlight)
	       ;; mu4e
	       (mu4e-unread-face :foreground ,pink :weight normal)
	       (mu4e-view-url-number-face :foreground ,purple)
	       (mu4e-highlight-face :background ,bg
				    :foreground ,yellow
				    :extend t)
	       (mu4e-header-highlight-face :background ,current
					   :foreground ,fg
					   :underline nil :weight bold
					   :extend t)
	       (mu4e-header-key-face :inherit message-mml)
	       (mu4e-header-marks-face :foreground ,purple)
	       (mu4e-cited-1-face :foreground ,purple)
	       (mu4e-cited-2-face :foreground ,orange)
	       (mu4e-cited-3-face :foreground ,comment)
	       (mu4e-cited-4-face :foreground ,fg2)
	       (mu4e-cited-5-face :foreground ,fg3)
               (mu4e-cited-6-face                            :inherit font-lock-comment-delimiter-face :weight normal :slant t)
	       (mu4e-cited-7-face                            :inherit font-lock-type-face :weight normal :slant t)
	       ;; org
	       (org-agenda-date :foreground ,cyan :underline nil)
	       (org-agenda-dimmed-todo-face :foreground ,comment)
	       (org-agenda-done :foreground ,green)
	       (org-agenda-structure :foreground ,purple)
	       (org-block :foreground ,orange)
	       (org-code :inherit font-lock-constant-face :foreground ,yellow)
	       (org-column :background ,bg4)
	       (org-column-title :inherit org-column :weight bold :underline t)
	       (org-date :foreground ,cyan :underline t)
	       (org-document-info :foreground ,alt-blue)
	       (org-document-info-keyword :foreground ,comment)
	       (org-document-title :inherit font-lock-string-face :weight bold :foreground ,orange
				   ,@(when poly-dark-enlarge-headings
				       (list :height poly-dark-height-doc-title)))
	       (org-done :foreground ,green)
	       (org-ellipsis :foreground ,comment)
	       (org-footnote :foreground ,alt-blue)
	       (org-formula :foreground ,pink)
	       (org-headline-done :foreground ,comment
				  :weight normal :strike-through t)
	       (org-hide :foreground ,bg :background ,bg)
	       (org-level-1 :inherit bold :foreground ,pink
			    ,@(when poly-dark-enlarge-headings
				(list :height poly-dark-height-title-1)))
	       (org-level-2 :inherit bold :foreground ,purple
			    ,@(when poly-dark-enlarge-headings
				(list :height poly-dark-height-title-2)))
	       (org-level-3 :weight normal :foreground ,green
			    ,@(when poly-dark-enlarge-headings
				(list :height poly-dark-height-title-3)))
	       (org-level-4 :weight normal :foreground ,yellow)
	       (org-level-5 :weight normal :foreground ,cyan)
	       (org-level-6 :weight normal :foreground ,orange)
	       (org-level-7 :weight normal :foreground ,alt-blue)
	       (org-level-8 :weight normal :foreground ,fg)
	       (org-link :foreground ,cyan :underline t)
	       (org-priority :foreground ,cyan)
	       (org-scheduled :foreground ,green)
	       (org-scheduled-previously :foreground ,yellow)
	       (org-scheduled-today :foreground ,green)
	       (org-sexp-date :foreground ,fg4)
	       (org-special-keyword :foreground ,yellow)
	       (org-table :foreground ,purple)
	       (org-tag :foreground ,pink :weight bold :background ,bg2)
	       (org-todo :foreground ,orange :weight bold :background ,bg2)
	       (org-upcoming-deadline :foreground ,yellow)
	       (org-warning :weight bold :foreground ,pink)
	       ;; outline
	       (outline-1 :foreground ,pink)
	       (outline-2 :foreground ,purple)
	       (outline-3 :foreground ,green)
	       (outline-4 :foreground ,yellow)
	       (outline-5 :foreground ,cyan)
	       (outline-6 :foreground ,orange)
	       ;; powerline
	       (powerline-evil-base-face :foreground ,bg2)
	       (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,yellow)
	       (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,cyan)
	       (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,purple)
	       (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,green)
	       (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,pink)
	       (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,red)
	       (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,orange)
	       ;; rainbow-delimiters
	       (rainbow-delimiters-depth-1-face :foreground ,fg)
	       (rainbow-delimiters-depth-2-face :foreground ,cyan)
	       (rainbow-delimiters-depth-3-face :foreground ,purple)
	       (rainbow-delimiters-depth-4-face :foreground ,pink)
	       (rainbow-delimiters-depth-5-face :foreground ,orange)
	       (rainbow-delimiters-depth-6-face :foreground ,green)
	       (rainbow-delimiters-depth-7-face :foreground ,yellow)
	       (rainbow-delimiters-depth-8-face :foreground ,alt-blue)
	       (rainbow-delimiters-unmatched-face :foreground ,orange)
	       ;; rpm-spec
	       (rpm-spec-dir-face :foreground ,green)
	       (rpm-spec-doc-face :foreground ,pink)
	       (rpm-spec-ghost-face :foreground ,purple)
	       (rpm-spec-macro-face :foreground ,yellow)
	       (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
	       (rpm-spec-package-face :foreground ,purple)
	       (rpm-spec-section-face :foreground ,yellow)
	       (rpm-spec-tag-face :foreground ,cyan)
	       (rpm-spec-var-face :foreground ,orange)
	       ;; show-paren
	       (show-paren-match-face :background unspecified
				      :foreground ,cyan
				      :weight bold)
	       (show-paren-match :background unspecified
				 :foreground ,cyan
				 :weight bold)
	       (show-paren-match-expression :inherit match)
	       (show-paren-mismatch :inherit font-lock-warning-face)
	       ;; slime
	       (slime-repl-inputed-output-face :foreground ,purple)
	       ;; spam
	       (spam :inherit gnus-summary-normal-read :foreground ,orange
		     :strike-through t :slant oblique)
	       ;; tab-bar & tab-line (since Emacs 27.1)
	       (tab-bar                        :background ,bg+2)
	       (tab-bar-tab-inactive           :inherit shadow
					       :background ,bg2)
	       (tab-bar-tab-ungrouped :inherit :background ,bg+2)
	       (tab-bar-tab :bold t :foreground ,blue :background ,bg
			    :box (:line-width 2 :color ,bg :style nil))
	       (tab-line :foreground ,purple :background ,current
			 :height 0.9 :inherit variable-pitch)
	       (tab-line-tab :foreground ,pink :background ,bg
			     :box (:line-width 2 :color ,bg :style nil))
	       (tab-line-tab-inactive :foreground ,purple :background ,bg2
				      :box (:line-width 2 :color ,bg2 :style nil))
	       (tab-line-tab-current :inherit tab-line-tab)
	       (tab-line-close-highlight :foreground ,red)
	       ;; term
	       (term :foreground ,fg :background ,bg)
	       (term-color-black :foreground ,bg :background ,bg)
	       (term-color-blue :foreground ,purple :background ,purple)
	       (term-color-cyan :foreground ,cyan :background ,cyan)
	       (term-color-green :foreground ,green :background ,green)
	       (term-color-magenta :foreground ,pink :background ,pink)
	       (term-color-red :foreground ,red :background ,red)
	       (term-color-white :foreground ,fg :background ,fg)
	       (term-color-yellow :foreground ,yellow :background ,yellow)

	       ;; undo-tree
	       (undo-tree-visualizer-current-face :foreground ,orange)
	       (undo-tree-visualizer-default-face :foreground ,fg2)
	       (undo-tree-visualizer-register-face :foreground ,purple)
	       (undo-tree-visualizer-unmodified-face :foreground ,fg)
	       ;; web-mode
	       (web-mode-builtin-face :inherit ,font-lock-builtin-face)
	       (web-mode-comment-face :inherit ,font-lock-comment-face)
	       (web-mode-constant-face :inherit ,font-lock-constant-face)
	       (web-mode-doctype-face :inherit ,font-lock-comment-face)
	       (web-mode-function-name-face :inherit ,font-lock-function-name-face)
	       (web-mode-html-attr-name-face :foreground ,purple)
	       (web-mode-html-attr-value-face :foreground ,green)
	       (web-mode-html-tag-face :foreground ,pink :weight bold)
	       (web-mode-keyword-face :foreground ,pink)
	       (web-mode-string-face :foreground ,yellow)
	       (web-mode-type-face :inherit ,font-lock-type-face)
	       (web-mode-warning-face :inherit ,font-lock-warning-face)
	       ;; which-func
	       (which-func :inherit ,font-lock-function-name-face)
	       ;; whitespace
	       (whitespace-big-indent :background ,red :foreground ,red)
	       (whitespace-empty :background ,orange :foreground ,red)
	       (whitespace-hspace :background ,bg3 :foreground ,comment)
	       (whitespace-indentation :background ,orange :foreground ,red)
	       (whitespace-line :background ,bg :foreground ,pink)
	       (whitespace-newline :foreground ,comment)
	       (whitespace-space :background ,bg :foreground ,comment)
	       (whitespace-space-after-tab :background ,orange :foreground ,red)
	       (whitespace-space-before-tab :background ,orange :foreground ,red)
	       (whitespace-tab :background ,bg2 :foreground ,comment)
	       (whitespace-trailing :inherit trailing-whitespace)
	       ;; yard-mode
	       (yard-tag-face :inherit ,font-lock-builtin-face)
               (persp-selected-face :foreground ,purple :weight bold)
               ;;;; persp-mode
	       (persp-face-lighter-default :foreground ,fg3 :weight bold)
	       (persp-face-lighter-buffer-not-in-persp :foreground ,alt-blue)
	       (persp-face-lighter-nil-persp :foreground ,comment)

	       (meow-keypad-indicator          :inherit mode-line :foreground ,red)
               (meow-insert-indicator          :inherit mode-line :foreground ,green)
               (meow-normal-indicator          :inherit mode-line :foreground ,yellow)
               (meow-motion-indicator          :inherit mode-line :foreground ,blue)
               (meow-beacon-indicator          :inherit mode-line :foreground ,purple)

               (meow-keypad-cursor              :foreground ,bg :background ,red)
               (meow-insert-cursor             :foreground ,bg :background ,green)
               (meow-normal-cursor :foreground ,bg  :background ,yellow)
 	       (meow-motion-cursor :foreground ,bg  :background ,blue)
	       (meow-beacon-cursor :foreground ,bg :background ,purple)

               (Man-overstrike :inherit font-lock-type-face :bold t)
               (Man-reverse :inherit highlight)
               (Man-underline :inherit font-lock-keyword-face :underline t)

               (apropos-keybinding :inherit font-lock-keyword-face)
	       (apropos-label  :inherit font-lock-type-face)
	       (apropos-match  :inherit font-lock-function-name-face)
	       (apropos-property :inherit font-lock-string-face)
	       (apropos-symbol  :inherit font-lock-keyword-face)

               (lsp-face-highlight-read :underline t :background unspecified :foreground unspecified)
               (lsp-face-highlight-write :underline t :background unspecified :foreground unspecified)
               (lsp-face-highlight-textual :underline t :background unspecified :foreground unspecified)
               (lsp-ui-peek-highlight :inherit nil :background unspecified :foreground unspecified :weight semi-bold :box (:line-width -1))
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
