;; No white flash
(add-to-list 'default-frame-alist '(background-color . "#0e1014"))

;; Start maximized
(push '(fullscreen . maximized) default-frame-alist)

;; No GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; Tab line config
(setq tab-line-separator " | ")
(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)
(set-face-attribute 'tab-line nil :weight 'normal :background "#21252d" :foreground "#6c7d8e" :height 1.0 :box nil :family "Iosevka ss04")
