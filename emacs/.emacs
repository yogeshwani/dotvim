; Start server for emacsclient
(server-start)

; Function to toggle betwwen horizontal and vertical split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

; Show column in status bar
(setq column-number-mode 1)

; Highlight parens
(show-paren-mode 1)
(electric-pair-mode 1)

; Stop creating backup~ files
(setq make-backup-files nil)

; Stop creating #autosave# files
(setq auto-save-default nil)

; keep a list of recently opened files
(recentf-mode 1)

; Setup GDB to show all windows (source, locals, registers, etc.)
(setq gdb-many-windows t)

; Setup fill column per mode
(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (set-fill-column 80)))

; Setup TAB to simply insert a TAB char
;(global-set-key (kbd "TAB") 'self-insert-command)

; When indenting insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

; Set the style to use for comment-region
(setq comment-style 'multi-line)

; ediff in vertical
(setq ediff-split-window-function 'split-window-horizontally)

;; Setup package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Setup line-mode stuff
; Without this it shows ?? in files with long lines
(setq line-number-display-limit-width 2000000)

;; Setup c-mode stuff
; Configure c-mode default style
(c-add-style "mine"
             '("linux"
               (c-basic-offset . 4)
               (c-offsets-alist
                (innamespace . -)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +))))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "mine")))

; Setup files ending in “.ino” to open in c-mode
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; Setup whitespace mode
; Set C/C++ modes to highlight trailing spaces and tabs
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-global-modes '(c-mode c++-mode python-mode))
(setq whitespace-style '(face tabs lines-tail trailing))


;; Setup Helm mode
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/helm")

(require 'helm-config)
(helm-mode 1)

; A few handy shortcuts
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

; Setup ctags-auto-update minor mode (from helm-etags-plus)
(add-to-list 'load-path "~/.emacs.d/helm-etags-plus")
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)

(autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
(global-set-key (kbd "C-c e") 'ctags-update)

(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)

; Setup Helm window move to meta (move with M-right M-left)
(windmove-default-keybindings 'meta)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; xcscope minor mode setup
(add-to-list 'load-path "~/.emacs.d/xcscope.el")
(require 'xcscope)
(cscope-setup)
(setq cscope-use-relative-paths t)

;; Org mode setup
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; Enable selecting text with shift
(setq org-support-shift-select t)

(require 'epa-file)
(epa-file-enable)

;; Setup projectile mode
(add-to-list 'load-path "~/.emacs.d/dash")
(add-to-list 'load-path "~/.emacs.d/projectile")
;(add-hook 'after-init-hook #'projectile-global-mode)
(require 'dash)
(require 'projectile)
(projectile-global-mode)

;; UI stuff
; Set default font to 9 pt
(set-face-attribute 'default nil :height 90)

; Disable toolbar and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; Add themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-boron-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/amelie-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/ample-zen")

; Load chosen theme
(load-theme 'Amelie t)

(custom-set-variables
 '(custom-safe-themes (quote ("a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" "6f139538f6f0838e04f53804f11a63ebaac743c660d15a3cbdb992097a44bef4" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" default)))
 '(safe-local-variable-values (quote ((gud-gdb-command-name . "arm-none-eabi-gdb -i=mi --annotate=3")))))


(setq cscope-search-list (list '("builds/mac/mac_condord01_wlanlite_hardmac_ram_sdio_gcc" . nil)))
