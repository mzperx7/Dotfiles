;; Load theme
(load-theme 'dark-emacs :no-confirm)

;; Load font
(add-to-list 'default-frame-alist '(font . "Iosevka ss04-19"))

;; No bells
(setq ring-bell-function 'ignore)

;; No confirmations
(setq confirm-kill-processes nil)

;; No tooltips
(setq tooltip-mode nil)

;; Enable windmove
(windmove-default-keybindings)
(windmove-default-keybindings 'meta)

;; Smooth scrolling
(setq scroll-step 1)

;; Set cursor
(blink-cursor-mode 0)

;; Indentation config
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Require final new line
(setq require-final-newline t)

;; Set minibuffer
(fido-mode 1)
(column-number-mode 1)

;; Line numbers
(global-display-line-numbers-mode t)

;; Delete tailing white-spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Overwrite selection
(delete-selection-mode 1)

;; Electric pair
(electric-pair-mode t)

;; Only y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Directories first
(setq dired-listing-switches "-aBhl --group-directories-first")

;; CUA mode
(cua-mode t)

;; No ESC as a modifier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Config for C
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(load "/usr/share/emacs/site-lisp/clang-format-14/clang-format.el")

;; Tab line config - background behind tabs, active tab in another window, active tab, inactive tab, mousover
(global-tab-line-mode t)
(set-face-attribute 'tab-line-tab-current nil :background "#21252d" :foreground "#b3b8c2" :box nil)
(set-face-attribute 'tab-line-tab-inactive nil :background "#21252d" :foreground "#6c7d8e" :box nil)
(set-face-attribute 'tab-line-highlight nil :background "#21252d" :foreground 'unspecified)

;; Remove buffers after a file is open
(defun remove-buffers ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (if (get-buffer "*Messages*")
      (kill-buffer "*Messages*"))
    (if (get-buffer "*Async-native-compile-log*")
      (kill-buffer "*Async-native-compile-log*"))
  )
(add-hook 'find-file-hook 'remove-buffers)

;; Kill current buffer
(defun volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))
 (global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; Function to find Makefile for compilation
(defun find-and-compile ()
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
  (with-temp-buffer
    (cd (locate-dominating-file default-directory "Makefile"))
    (compile "make -k"))))

;; Load C language grammar
(use-package c-ts-mode
  :ensure t
  :custom (c-ts-mode-indent-offset 4)
  :hook (c-ts-mode . eglot-ensure)
  :bind (:map c-ts-mode-map
              ("<f5>" . 'find-and-compile))
              ("<f6>" . 'clang-format-buffer))

;; Load Company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

;; Load iedit: https://github.com/victorhge/iedit
(use-package iedit
  :load-path "/home/ko/.emacs.d/iedit")

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dark-emacs))
 '(custom-safe-themes
   '("96714bea01c6e9ca930e3305a7e7a7646e08d6ffff1386ff5c1396691d5fa6eb" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
