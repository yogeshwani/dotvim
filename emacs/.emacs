(set-face-attribute 'default nil :height 90)

; disable toolbar and menubar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-global-modes '(c-mode c++-mode))
(setq whitespace-style '(face tabs lines-tail trailing))

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

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region 'disabled nil)

(setq column-number-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(recentf-mode 1) ; keep a list of recently opened files

;(setq-default  indent-tabs)


(setq-default indent-tabs-mode nil)

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

(setq comment-style 'multi-line)

(defun insert-file-name ()
    "Insert the full path file name into the current buffer."
      (interactive)
        (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

(server-start)

(global-set-key (kbd "C-i") buffer-file-name)
;; [Facultative] Only if you have installed async.
(add-to-list 'load-path "~/.emacs.d/emacs-async")

(add-to-list 'load-path "~/.emacs.d/helm")

(setq column-number-mode t)
(require 'helm-config)

(global-set-key (kbd "TAB") 'self-insert-command)
(add-to-list 'load-path "~/.emacs.d/helm-etags-plus")
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)

(autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
(global-set-key (kbd "C-c e") 'ctags-update)


(windmove-default-keybindings 'meta)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/ample-zen")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" "6f139538f6f0838e04f53804f11a63ebaac743c660d15a3cbdb992097a44bef4" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" default)))
 '(safe-local-variable-values (quote ((gud-gdb-command-name . "arm-none-eabi-gdb -i=mi --annotate=3")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-boron-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/amelie-theme")

(setq gdb-many-windows t)

(load-theme 'Amelie t)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-support-shift-select t)
