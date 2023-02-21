;; Package Management
(require 'package)
(add-to-list 'package-archives '("elpa"  . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

;; Packages
(use-package company
  :config
  (global-company-mode 1)
  ; use "\t" instead of (kbd "<tab>"), so it's not triggered in the mini-buffer
  (global-set-key "\t" #'company-indent-or-complete-common))

(use-package magit)

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

; TODO: https://github.com/joaotavora/yasnippet

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
