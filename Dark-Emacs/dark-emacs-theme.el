(deftheme dark-emacs "Dark color theme")

(let ((ok-bg-1       "#0e1014")
      (ok-bg-2       "#21252d")
      (ok-bg-3       "#49505e")
      (ok-fg-1       "#6c7d8e")
      (ok-fg-2       "#b3b8c2")
      (ok-fg-3       "#e5e7ea")
      (ok-red-1      "#8e1201")
      (ok-red-2      "#da1c02")
      (ok-green      "#6d9a79")
      (ok-blue       "#668dbb")
      (ok-yellow-1   "#99918a")
      (ok-yellow-2   "#d2c88e")
      (ok-cyan       "#017d8e"))

  (custom-theme-set-variables 'dark-emacs '(frame-background-mode (quote dark)))
  (custom-theme-set-faces 'dark-emacs

   ;; Basic Coloring
   `(border ((t ,(list :background ok-bg-1 :foreground ok-bg-2))))
   `(default ((t ,(list :foreground ok-fg-2 :background ok-bg-1))))
   `(cursor ((t (:background ,ok-green))))
   `(fringe ((t (:foreground ,ok-cyan))))
   `(vertical-border ((t ,(list :foreground ok-fg-2))))
   `(link ((t (:foreground ,ok-green :underline t))))
   `(link-visited ((t (:foreground ,ok-cyan :underline t))))
   `(match ((t (:background ,ok-fg-3))))
   `(shadow ((t (:foreground ,ok-bg-3))))
   `(minibuffer-prompt ((t (:foreground ,ok-blue))))
   `(region ((t (:background ,ok-bg-2))))
   `(secondary-selection ((t (:background ,ok-bg-3))))
   `(trailing-whitespace ((t ,(list :foreground ok-cyan :background ok-bg-1))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground ok-green :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground ok-red-2 :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground ok-green :inherit 'unspecified))))
   `(compilation-mode-line-fail ((t ,(list :foreground ok-red-1 :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,ok-red-1))))

   ;; Diff
   `(diff-removed ((t (:foreground ,ok-red-2))))
   `(diff-added ((t (:foreground ,ok-green))))

   ;; Dired
   `(dired-directory ((t (:foreground ,ok-blue))))
   `(dired-ignored ((t ,(list :foreground ok-fg-1 :inherit 'unspecified))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,ok-fg-1))))
   `(eshell-ls-directory ((t (:foreground ,ok-blue))))
   `(eshell-ls-executable ((t (:foreground ,ok-green))))
   `(eshell-ls-symlink ((t (:foreground ,ok-cyan))))

   ;; Font Lock
   `(font-lock-bracket-face ((t (:foreground ,ok-fg-3))))
   `(font-lock-builtin-face ((t (:foreground ,ok-fg-2))))
   `(font-lock-comment-face ((t (:foreground ,ok-yellow-1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,ok-yellow-1))))
   `(font-lock-constant-face ((t (:foreground ,ok-yellow-2))))
   `(font-lock-delimiter-face ((t (:foreground ,ok-fg-3))))
   `(font-lock-doc-face ((t (:foreground ,ok-yellow-2))))
   `(font-lock-doc-string-face ((t (:foreground ,ok-yellow-2))))
   `(font-lock-function-name-face ((t (:foreground ,ok-fg-2))))
   `(font-lock-keyword-face ((t (:foreground ,ok-fg-1))))
   `(font-lock-negation-char-face  ((t (:foreground ,ok-yellow-2))))
   `(font-lock-number-face ((t (:foreground ,ok-yellow-2))))
   `(font-lock-operator-face ((t (:foreground ,ok-fg-3))))
   `(font-lock-preprocessor-face ((t (:foreground ,ok-fg-1))))
   `(font-lock-punctuation-face ((t (:foreground ,ok-fg-2))))
   `(font-lock-string-face ((t (:foreground ,ok-yellow-2))))
   `(font-lock-type-face ((t (:foreground ,ok-fg-1))))
   `(font-lock-variable-name-face ((t (:foreground ,ok-fg-2))))
   `(font-lock-warning-face ((t (:foreground ,ok-red-1))))

   ;; Line Highlighting
   `(highlight ((t (:background ,ok-bg-3))))

   ;; Line numbers
   `(line-number ((t (:inherit default :foreground ,ok-bg-2))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,ok-fg-2))))

   ;; Mode Line
   `(mode-line ((t ,(list :background ok-bg-2 :foreground ok-fg-2))))
   `(mode-line-buffer-id ((t ,(list :background ok-bg-2 :foreground ok-fg-2))))
   `(mode-line-inactive ((t ,(list :background ok-bg-2 :foreground ok-fg-1))))

   ;; Search
   `(isearch ((t (:background ,ok-bg-3))))
   `(isearch-fail ((t ,(list :foreground ok-fg-3 :background ok-red-1))))
   `(isearch-lazy-highlight-face ((t ,(list :foreground ok-fg-2 :background ok-bg-3))))
   `(set-face-attribute 'lazy-highlight nil :foreground ok-fg-2 :background ok-bg-3)

   ;; Show Paren
   `(show-paren-match ((t (:background ,ok-blue))))
   `(show-paren-mismatch ((t (:background ,ok-red-2))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,ok-fg-1))))
   `(font-latex-italic-face ((t (:foreground ,ok-fg-1 :italic t))))
   `(font-latex-math-face ((t (:foreground ,ok-yellow-2))))
   `(font-latex-sectioning-0-face ((t ,(list :foreground ok-blue))))
   `(font-latex-sectioning-1-face ((t ,(list :foreground ok-blue))))
   `(font-latex-sectioning-2-face ((t ,(list :foreground ok-blue))))
   `(font-latex-sectioning-3-face ((t ,(list :foreground ok-blue))))
   `(font-latex-sectioning-4-face ((t ,(list :foreground ok-blue))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground ok-blue))))
   `(font-latex-slide-title-face ((t (:foreground ,ok-blue))))
   `(font-latex-string-face ((t (:foreground ,ok-yellow-2))))
   `(font-latex-warning-face ((t (:foreground ,ok-red-2))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,ok-fg-1 :background ,ok-bg-2))))
   `(company-tooltip-annotation ((t (:foreground ,ok-yellow-1 :background ,ok-bg-2))))
   `(company-tooltip-annotation-selection ((t (:foreground ,ok-yellow-1 :background ,ok-bg-3))))
   `(company-tooltip-selection ((t (:foreground ,ok-fg-1 :background ,ok-bg-3))))
   `(company-tooltip-mouse ((t (:background ,ok-bg-3))))
   `(company-tooltip-common ((t (:foreground ,ok-green))))
   `(company-tooltip-common-selection ((t (:foreground ,ok-green))))
   `(company-scrollbar-bg ((t (:background ,ok-bg-2))))
   `(company-scrollbar-fg ((t (:background ,ok-bg-2))))
   `(company-preview ((t (:background ,ok-green))))
   `(company-preview-common ((t (:foreground ,ok-green :background ,ok-bg-2))))
   ))

(provide-theme 'dark-emacs)
