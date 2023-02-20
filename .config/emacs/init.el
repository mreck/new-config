;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-list '(company magit smex))
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

(setq initial-scratch-message ""
      inhibit-startup-message t
      visible-bell nil
      compilation-ask-about-save nil
      backup-directory-alist '(("." . "~/.emacs-backups")))

(load-theme 'deeper-blue t)
(set-frame-font "Droid Sans Mono 13" nil t)

;; Interactivity
(ido-mode t)
; (ido-everywhere t)

(setq ido-enable-flex-matching t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Recent Files
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Clean up White Space
(add-hook 'before-save-hook 'whitespace-cleanup)

;; File Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; Editing
(setq tab-width 4
      show-trailing-whitespace t
      indent-tabs-mode nil)

;; Auto-Complete
(global-company-mode 1)
; use "\t" instead of (kbd "<tab>"), so it's not triggered in the mini-buffer
(global-set-key "\t" #'company-indent-or-complete-common)

;; Snippets
; TODO: https://github.com/joaotavora/yasnippet
