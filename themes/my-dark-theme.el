;;; -*- mode: emacs-lisp; eval: (rainbow-mode); lexical-binding: t -*-

(deftheme my-dark "my dark theme.")

(defvar my-dark-theme-use-italic t
  "Non-nil means use italic for comment and docstring.")

(defvar my-dark-theme-header-scales
  '(1.6 1.4 1.2 1.2 1.2 1.1 1.0)
  "Scales for headers.")

(defvar my-dark-theme-main-color "#00AAAA"
  "The main color used for some places.
You may want to set this to window's border color.")

(defface tabnine-face '((t (:inherit font-lock-string-face)))
  "tabnine face")

(let ((fg "#F8F8F2")
      (fg+1 "#CCCCCC")
      (fg-1 "#909090")
      (bg "#22212C")
      (bg-1 "#17161D") ;; mode-line
      (bg+1 "#454158")
      (bg+2 "#303030")
      (bg+3 "#404040")
      (bg+4 "#505050")
      (bglighter "#393649")
      (bglight "#2E2B3B")
      (bgdark "#17161D")
      (bgdarker "#0B0B0F")
      (black "#17161D")
      (white "#E0E0E0")
      (italic my-dark-theme-use-italic)
      (yellow "#FFFF80")
      (red "#FF80BF")
      (blue "#009F9F")
      (green "#39BA7E")
      (green2 "#8AFF80")
      (purple "#9580FF")
      (light-purple "#B28CE2")
      (orange "#FC9F4E")
      (brown "#CFA300")
      (region "#454158")
      (region2 "#350035")
      (comment "#7970A9")
      (match-bg "#effb7b")
      (match-fg "#000000")
      (magenta "#ff05fa")
      (main my-dark-theme-main-color))

  (custom-theme-set-faces
   `my-dark
   ;; We don't specify default foreground/background in TTY.
   `(default                        ((((type tty)))
                                     (((type graphic))
                                      :background ,bg
                                      :foreground ,fg)))
   ;; Basics

   `(cursor                         ((t (:background ,white))))
   `(region                         ((t (:background ,region))))
   ;; `(hl-line                        ((((type graphic)) :background ,bg+1)
   ;;                                   (((type tty)))))
   `(fringe                         ((t (:background ,bg))))
   `(show-paren-match               ((t (:underline ,green))))
   `(highlight                      ((t (:background ,bg+2))))
   ;; `(highlight                      ((t (:background ,hilight-bg :foreground ,hilight-fg))))
   `(button                         ((t (:foreground ,blue :underline t))))
   `(vertical-border                ((t ())))
   `(window-divider                 ((t (:foreground ,bg+3))))
   `(window-divider-first-pixel     ((t (:foreground ,bg+1))))
   `(window-divider-last-pixel      ((t (:foreground ,bg+1))))
   `(line-number                    ((t (:foreground ,bg+3 :inherit default))))
   `(line-number-current-line       ((((type tty)) :foreground ,yellow)
                                     (((type graphic)) :inherit default :foreground ,yellow :background ,bg+1)))
   `(parenthesis                    ((t (:foreground ,fg-1))))
   `(completions-common-part        ((t ())))
   `(minibuffer-prompt              ((t (quote (read-only nil
							  cursor-intangible t
							  face minibuffer-prompt));; ()
					)))
   ;; `(lazy-highlight                 ((t (:background ,bg+3))))
   ;; `(lazy-highlight                 ((t (:background nil))))
   ;; `(match                          ((t (:background ,bg+2))))
   `(match                          ((t (:background ,match-bg :foreground ,match-fg))))
   `(xref-match                     ((t (:inherit (match));; (:background ,match-bg :foreground ,match-fg)
					)))
   `(secondary-selection            ((t (:background ,region2 :extend t))))

   ;; ISearch
   `(isearch                        ((t (:background ,green :foreground ,black))))
   `(isearch-fail                   ((t (:backgronud ,red :foreground ,orange))))

   ;; Font Locks
   `(font-lock-comment-face         ((t (:foreground ,comment :italic ,italic))))
   `(font-lock-comment-delimiter-face         ((t (:foreground ,comment :italic ,italic))))
   `(font-lock-string-face          ((t (:foreground ,yellow))))
   `(font-lock-doc-face             ((t (:foreground ,blue :italic ,italic))))
   `(font-lock-builtin-face         ((t (:foreground ,purple))))
   `(font-lock-type-face            ((t (:forground ,green))))
   `(font-lock-variable-name-face   ((t (:foreground ,white))))
   `(font-lock-operator-face   ((t (:foreground ,purple))))
   `(font-lock-keyword-face         ((t (:foreground ,red))))
   `(font-lock-number-face         ((t (:foreground ,purple))))
   `(font-lock-delimiter-face         ((t (:foreground ,purple))))
   `(font-lock-bracket-face         ((t (:foreground ,red))))
   `(font-lock-escape-face         ((t (:foreground ,red))))
   `(font-lock-property-face         ((t (:foreground ,red))))
   `(font-lock-constant-face        ((t (:foreground ,purple))))
   `(font-lock-function-name-face   ((t (:foreground ,green2))))
   `(font-lock-warning-face         ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face    ((t (:inherit font-lock-constant-face))))

   `(compilation-info               ((t (:inherit font-lock-function-name-face))))
   `(compilation-warning            ((t (:inherit font-lock-warning-face))))
   `(warning                        ((t (:inherit font-lock-warning-face))))

   ;; IMenu
   `(imenu-list-entry-face-0          ((t ())))
   `(imenu-list-entry-subalist-face-0 ((t (:bold t))))

   ;; Mode Line
   `(mode-line                      ((t (:background ,bg-1))))
   `(mode-line-inactive             ((t (:background ,bg+1))))


   ;; Yascroll
   `(yascroll:thumb-fringe          ((t (:background ,main :foreground ,main))))
   `(yascroll:thumb-text-area       ((t (:background ,main :foreground ,main))))

   ;; Company
   `(company-tooltip-common         ((t (:bold t))))
   `(company-tooltip-common-selection ((t (:bold t))))
   `(company-tooltip                ((t (:background ,bg+2))))
   `(company-tooltip-selection      ((t (:background ,bg+3))))
   `(company-tooltip-annotation     ((t (:foreground ,blue))))
   `(company-scrollbar-bg           ((t (:background ,bg+2 :height 0.3))))
   `(company-scrollbar-fg           ((t (:background ,bg+4 :height 0.3))))
   `(company-template-field         ((t (:inherit yas-field-highlight-face))))

   `(corfu-default                ((t (:background "#151321"))))
   `(corfu-current                ((t (:background ,region))))
   `(corfu-border                ((t (:background ,bg+1))))
   `(tabnine-face                ((t (:forground ,white))))

   ;; Yasnippet
   `(yas-field-highlight-face       ((t (:background ,region2))))

   ;; Meow
   `(meow-keypad-indicator          ((t (:foreground "black" :background ,red))))
   `(meow-insert-indicator          ((t (:foreground "black" :background ,green))))
   ;; `(meow-normal-indicator          ((t (:foreground "black" :background ,yellow))))
   `(meow-normal-indicator          ((t (:foreground "black" :background ,yellow))))
   `(meow-motion-indicator          ((t (:foreground "black" :background ,blue))))
   `(meow-beacon-indicator          ((t (:foreground "black" :background ,purple))))
   `(meow-keypad-cursor             ((t ())))
   `(meow-insert-cursor             ((t (:background ,green))))
   `(meow-normal-cursor             ((t ())))
   `(meow-motion-cursor             ((t ())))

   ;; Cider
   ;;
   `(cider-result-overlay-face      ((t (:background "black"))))
   `(cider-repl-stderr-face         ((t (:foreground ,blue))))
   `(cider-repl-stdout-face         ((t (:foreground ,fg-1))))

   ;; Clojure
   ;;
   `(clojure-character-face         ((t (:foreground ,purple))))

   ;; Ivy
   `(ivy-highlight-face             ((t ())))
   `(ivy-yanked-word                ((t (:background "yellow" :foreground "black"))))
   `(ivy-remote                     ((t ())))
   `(ivy-current-match              ((t (:foreground ,bg :background ,main))))
   `(ivy-minibuffer-match-highlight ((t ())))
   `(ivy-minibuffer-match-face-1    ((t ())))
   `(ivy-minibuffer-match-face-2    ((t ())))
   `(ivy-minibuffer-match-face-3    ((t ())))
   `(ivy-minibuffer-match-face-4    ((t ())))
   `(counsel-outline-default        ((t ())))
   `(swiper-background-match-face-1 ((t (:inherit hl-line))))
   `(swiper-background-match-face-2 ((t (:inherit hl-line))))
   `(swiper-background-match-face-3 ((t (:inherit hl-line))))
   `(swiper-background-match-face-4 ((t (:inherit hl-line))))
   `(swiper-match-face-1            ((t (:foreground "white"))))
   `(swiper-match-face-2            ((t (:foreground "white"))))
   `(swiper-match-face-3            ((t (:foreground "white"))))
   `(swiper-match-face-4            ((t (:foreground "white"))))

   ;; Selectrum
   `(selectrum-current-candidate    ((t (:foreground ,main :inverse-video t))))

   ;; Magit
   `(magit-diff-file-heading-highlight ((t (:background ,bg+1))))
   `(magit-section-highlight           ((t (:background ,bg+1))))
   `(magit-diff-removed             ((t (:inherit font-lock-string-face))))
   `(magit-diff-added               ((t (:inherit font-lock-comment-face))))
   `(magit-diff-removed-highlight   ((t (:inherit font-lock-string-face :background ,bg+2))))
   `(magit-diff-added-highlight     ((t (:inherit font-lock-comment-face :background ,bg+2))))
   `(magit-diff-highlight           ((t (:background ,bg+1))))
   `(magit-diff-context-highlight   ((t (:background ,bg+1))))

   ;; SMerge
   `(smerge-refined-added           ((t (:background "#253325"))))
   `(smerge-lower                   ((t (:background "#173017"))))

   ;; Diff-hl
   `(diff-hl-insert                 ((t (:foreground ,green :background ,green))))
   `(diff-hl-change                 ((t (:foreground ,blue :background ,blue))))
   `(diff-hl-delete                 ((t (:foreground ,red :background ,red))))

   ;; Term
   `(term-color-blue                ((t (:foreground ,blue :background ,blue))))
   `(term-color-green               ((t (:foreground ,green :background ,green))))
   `(term-color-red                 ((t (:foreground ,red :background ,red))))

   ;; Popup
   `(popup-tip-face                 ((t (:background ,bg+4 :foreground ,fg))))
   `(popup-isearch-match            ((t (:background ,brown :foreground "black"))))

   `(tooltip                 ((t (:background ,bg+4 :foreground ,fg))))
   ;; `(tooltip ((t (:foreground ,black :background ,yellow))))
   `(dired-directory                ((t (:foreground ,light-purple))))
   `(web-mode-html-attr-name-face   ((t ())))
   `(web-mode-html-tag-face         ((t ())))

   ;; Emacs Rime
   `(rime-preedit-face              ((t (:underline ,blue :background ,bg+2))))
   `(rime-cursor-face               ((t (:inherit font-lock-constant-face))))
   `(rime-indicator-face            ((t (:foreground ,purple))))
   `(rime-indicator-dim-face        ((t (:foreground ,bg+4))))

   ;; Web Mode
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   `(web-mode-symbol-face           ((t (:foreground ,purple))))
   `(css-selector                   ((t (:foreground ,purple))))

   ;; Markdown
   `(markdown-header-face-1         ((t (:bold t :height ,(nth 0 my-dark-theme-header-scales)))))
   `(markdown-header-face-2         ((t (:bold t :height ,(nth 1 my-dark-theme-header-scales)))))
   `(markdown-header-face-3         ((t (:bold t :height ,(nth 2 my-dark-theme-header-scales)))))
   `(markdown-header-face-4         ((t (:bold t :height ,(nth 3 my-dark-theme-header-scales)))))
   `(markdown-header-face-5         ((t (:bold t :height ,(nth 4 my-dark-theme-header-scales)))))
   `(markdown-header-face-6         ((t (:bold t :height ,(nth 5 my-dark-theme-header-scales)))))
   `(markdown-header-face-7         ((t (:bold t :height ,(nth 6 my-dark-theme-header-scales)))))

   ;; Telega
   `(telega-entity-type-code        ((t (:inherit font-lock-string-face))))
   `(telega-msg-heading             ((t (:inherit hl-line))))
   `(telega-unmuted-count           ((t (:inherit font-lock-function-name-face))))

   ;; Org-mode
   `(org-todo                      ((t (:foreground ,yellow))))
   `(org-done                      ((t (:foreground ,blue))))
   `(org-headline-todo             ((t (:foreground ,fg+1))))
   `(org-headline-done             ((t (:foreground ,fg-1 :strike-through t))))
   `(org-table                      ((t (:foreground ,fg+1))))
   `(org-level-1                    ((t (:bold t :height ,(nth 0 my-dark-theme-header-scales)))))
   `(org-level-2                    ((t (:bold t :height ,(nth 1 my-dark-theme-header-scales)))))
   `(org-level-3                    ((t (:bold t :height ,(nth 2 my-dark-theme-header-scales)))))
   `(org-level-4                    ((t (:bold t :height ,(nth 3 my-dark-theme-header-scales)))))
   `(org-level-5                    ((t (:bold t :height ,(nth 4 my-dark-theme-header-scales)))))
   `(org-level-6                    ((t (:bold t :height ,(nth 5 my-dark-theme-header-scales)))))
   `(org-level-7                    ((t (:bold t :height ,(nth 6 my-dark-theme-header-scales)))))
   `(org-document-title             ((t (:inherit font-lock-string-face))))
   `(org-code                       ((t (:inherit font-lock-constant-face))))

   `(mu4e-unread-face                            ((t :inherit font-lock-keyword-face :weight bold)))
   `(mu4e-trashed-face                            ((t :inherit font-lock-comment-face :strike-through t)))
   `(mu4e-draft-face                              ((t :inherit font-lock-string-face)))
   `(mu4e-flagged-face                            ((t :inherit font-lock-constant-face :weight bold)))
   `(mu4e-replied-face                          ((t :inherit font-lock-builtin-face :weight normal :slant normal)))
   `(mu4e-forwarded-face                          ((t :inherit font-lock-builtin-face :weight normal :slant normal)))
   `(mu4e-header-face                               ((t :inherit default)))
   `(mu4e-related-face                              ((t :inherit default :slant italic)))
   `(mu4e-header-title-face                      ((t :inherit font-lock-type-face)))
   `(mu4e-header-highlight-face                  ((t :inherit hl-line :weight bold :underline t
						     ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(mu4e-header-marks-face                       ((t :inherit font-lock-preprocessor-face)))
   `(mu4e-header-key-face                        ((t :inherit message-header-name :weight bold)))
   `(mu4e-header-value-face                         ((t :inherit font-lock-type-face)))
   `(mu4e-special-header-value-face                  ((t :inherit font-lock-builtin-face)))
   `(mu4e-link-face                             ((t :inherit link)))
   `(mu4e-contact-face                          ((t :inherit font-lock-variable-name-face)))
   `(mu4e-highlight-face                         ((t :inherit highlight)))
   `(mu4e-title-face                             ((t :inherit font-lock-type-face :weight bold)))
   `(mu4e-modeline-face                           ((t :inherit font-lock-string-face :weight bold)))
   `(mu4e-footer-face                             ((t :inherit font-lock-comment-face)))
   `(mu4e-url-number-face                         ((t :inherit font-lock-constant-face :weight bold)))
   `(mu4e-system-face                             ((t :inherit font-lock-comment-face :slant italic)))
   `(mu4e-ok-face                                 ((t :inherit font-lock-comment-face :weight bold :slant normal)))
   `(mu4e-warning-face                            ((t :inherit font-lock-warning-face :weight bold :slant normal)))
   `(mu4e-compose-separator-face                  ((t :inherit message-separator :slant italic)))
   `(mu4e-region-code                             ((t (:background "DarkSlateGray"))))
   `(mu4e-attach-number-face                     ((t :inherit font-lock-variable-name-face :weight bold)))
   `(mu4e-cited-1-face                            ((t :inherit font-lock-builtin-face :weight normal :slant italic)))
   `(mu4e-cited-2-face                            ((t :inherit font-lock-preprocessor-face :weight normal :slant italic)))
   `(mu4e-cited-3-face                            ((t :inherit font-lock-variable-name-face :weight normal :slant italic)))
   `(mu4e-cited-4-face                            ((t :inherit font-lock-keyword-face :weight normal :slant italic)))
   `(mu4e-cited-5-face                            ((t :inherit font-lock-comment-face :weight normal :slant italic)))
   `(mu4e-cited-6-face                            ((t :inherit font-lock-comment-delimiter-face :weight normal :slant italic)))
   `(mu4e-cited-7-face                            ((t :inherit font-lock-type-face :weight normal :slant italic)))
   `(mu4e-compose-header-face                     ((t :inherit message-separator :slant italic)))
   `(mu4e-context-face                            ((t :inherit mu4e-title-face :weight bold)))
   ;; `(mu4e-moved-face                           ((t ())))
   `(mu4e-view-body-face                          ((t :inherit default)))
   `(mu4e-modeline-face                           ((t :inherit font-lock-string-face :weight bold)))

   ;; Treemacs
   `(treemacs-root-face             ((t (:inherit font-lock-function-name-face :height 1.4 :underline t))))

   `(fill-column-indicator          ((t (:foreground ,bg+2))))

   `(scroll-bar                     ((t (:foreground ,fg-1))))

   `(tab-bar                        ((t (:background ,bg+2))))
   `(tab-bar-tab                    ((t (:inverse-video t :bold t :background ,blue ;; :foreground ,fg
							:foreground ,bg+2
							))))
   `(tab-bar-tab-inactive           ((t (:inherit shadow))))
   ;; `(tab-bar-tab-ungrouped ((t (:inherit ,bg+2))))
   `(tab-bar-tab-group-current      ((t ())))
   `(tab-bar-tab-group-inactive     ((t ())))
   `(icomplete-first-match ((t (:inherit mode-line-emphasis))))
   `(mode-line-buffer-id ((t (:foreground "Light Blue"))))
   `(font-lock-variable-name-face ((t (:foreground "#50fa7b"))))
   ;; (highlight-indentation-face ((t (:inherit default :foreground "#878787"))))
   ;; `(hl-line ((t (:background ,bg+1))))
   `(orderless-match-face-0 ((t (:inherit font-lock-type-face :weight bold))))
   `(orderless-match-face-1 ((t (:inherit error :weight bold))))
   `(orderless-match-face-2 ((t (:inherit font-lock-string-face :weight bold))))
   `(orderless-match-face-3 ((t (:inherit font-lock-keyword-face :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground "#50fa7b"))))
   `(highlight-indentation-face ((t (:inherit default :foreground "#878787"))))
   `(hl-line ((t (:background "DodgerBlue4"))))
   `(mode-line ((t (:foreground ,purple :background ,red))))
   ;; `(mode-line-inactive ((t (:family "Noto Sans" :height 100))))

   ;; auto-highlight-symbol
   `(ahs-definition-face ((t (:foreground ,magenta :background unspecified
                                          :slant normal))))
   `(ahs-definition-face-unfocused ((t (:foreground ,magenta :background unspecified
                                                    :slant normal))))
   `(ahs-edit-mode-face ((t (:foreground ,bg+1 :background ,magenta))))
   `(ahs-face ((t (:foreground ,magenta :background unspecified))))
   `(ahs-face-unfocused ((t (:foreground ,magenta :background unspecified))))
   `(ahs-plugin-bod-face ((t (:foreground ,magenta :background unspecified ))))
   `(ahs-plugin-default-face ((t (:foreground ,magenta :background unspecified))))
   `(ahs-plugin-default-face-unfocused ((t (:foreground ,magenta :background unspecified))))
   `(ahs-plugin-whole-buffer-face ((t (:foreground ,magenta  :background unspecified))))
   `(ahs-warning-face ((t (:foreground ,red :weight bold))))
   )
  )

;; Adjust default font height when running in HiDPI screen.
(when (> (frame-pixel-width) 3000)
  (custom-set-faces '(corfu-default ((t (:height 1.3))))))

;; (set-face-attribute 'mode-line nil :inherit 'mode-line)
;; (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'my-dark)
;;; my-dark-theme.el ends here
