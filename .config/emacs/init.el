;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-list '(magit smex))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Minimal UI
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq compilation-ask-about-save nil)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(load-theme 'deeper-blue t)
(set-frame-font "Droid Sans Mono 13" nil t)

;; Interactivity
(ido-mode t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Clean up white space
(add-hook 'before-save-hook 'whitespace-cleanup)

;; File Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
