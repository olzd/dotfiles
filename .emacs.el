(defvar first-time t
  "Flag signifying this is the first time that .emacs.el has been evaled")

;; under UNIX
(if (not (equal system-type 'ms-dos))
    (progn
      (if first-time
          (server-start))))

;; set default file for the customize command
(setq custom-file "~/.emacs-custom.el")
(load "~/.emacs-custom.el")

;; set default encoding
(set-language-environment "UTF-8")

;; make text-mode the default mode for new buffers
(setq-default major-mode 'text-mode)

;; turn on auto fill mode in text mode (and related modes)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; better buffer management (switching)
(iswitchb-mode t)

;; SLIME configuration with SBCL
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")


;; ********** Display Settings ********** ;;


;; don't display menubar, toolbar and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; text lines limited to 80 characters
(setq fill-column 80)

;; set font for all windows
(add-to-list 'default-frame-alist '(font . "Droid SansMono-11"))

;; display column number
(column-number-mode t)

;; don't truncate lines that are too long when in 2 windows (or more) mode
(setq truncate-partial-width-windows nil)

;; disable beep
(setq ring-bell-function 'ignore)

;; display time
(display-time-mode t)

;; don't display startup screen
(setq inhibit-startup-screen t)

;; always highlight the current line
(global-hl-line-mode 1)

;; show line numbers, dynamically with spaces on either side
(global-linum-mode 1)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d")))
    ad-do-it))

(if first-time
    (setq auto-mode-alist
          (append '(("\\.cpp$" . c++-mode)
                    ("\\.hpp$" . c++-mode)
                    ("\\.lsp$" . lisp-mode)
                    ("\\.scm$" . scheme-mode)
                    ("\\.pl$" . perl-mode))
                  auto-mode-alist)))


;; ********** Term mode settings ********** ;;

;; don't store in history successive identical inputs
(setq comint-input-ignoredups t)

;; completion for all files
(setq shell-completion-execonly nil)

;; set prompts in the buffer to read-only mode
(setq comint-prompt-read-only t)

;; file name extensions to ignore in shell mode completion
(setq shell-completion '("~" "#" "%"))


;; **********  keybindings ********** ;;


;; Function keys
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'info)
(global-set-key [f3] 'repeat-complex-command)
(global-set-key [f4] 'advertised-undo)
(global-set-key [f5] 'eval-current-buffer)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'other-window)
(global-set-key [f8] 'find-file)
(global-set-key [f9] 'save-buffer)
(global-set-key [f10] 'next-error)
(global-set-key [f11] 'compile)
(global-set-key [f12] 'grep)

;; Mouse
(global-set-key [mouse-3] 'imenu)

;; treat 'y' as 'yes' and 'n' as 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; bind replace-string command to C-c s
(global-set-key (kbd "C-c s") 'replace-string)

;; bind replace-regexp command to C-c r
(global-set-key (kbd "C-c r") 'replace-regexp)

;; bind bury-buffer command (cycle through buffers) to C-c b
(global-set-key (kbd "C-c b") 'bury-buffer)

;; bind kill-this-buffer command to C-c k
(global-set-key (kbd "C-c k") 'kill-this-buffer)

;; bind make command to C-c m
(global-set-key (kbd "C-c m") 'compile)

;; bind gdb command to C-c g
(global-set-key (kbd "C-c g") 'gdb)

;; auto-indent on return key (instead of C-j)
;(add-hook 'prog-mode-hook '(lambda ()
;                             (local-set-key (kbd "RET") 'newline-and-indent)))


;; ********** C configuration ********** ;;


;; set default style to K&R
(setq c-default-style "k&r")

;; set indentation width to 4
(setq c-basic-offset 4)

;; only use spaces to indent
(setq indent-tabs-mode nil)


;; ********** Packages Management ********** ;;

(require 'desktop)
(require 'tar-mode)

(require 'whitespace)
(setq lines-tail t)
(setq show-trailing-whitespaces t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Icicles
(icy-mode 1)

;; Better-defaults
;

;; Smex
(setq smex-save-file "~/.emacs.d/.smex-items")
(setq smex-auto-update 60)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

;; python-mode package setup
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.al_completions('''%s'''))\n")

;; Color theme
;;(setq color-theme-is-global t)
;;(color-theme-initialize)
(load-theme 'monokai t)

;; auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start nil)
(setq-default ac-dwim nil)

(setq tab-always-indent 'complete) ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

(setq c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

(defun sanityinc/auto-complete-at-point ()
  (when (and (not (minibufferp))
(fboundp 'auto-complete-mode)
auto-complete-mode)
    (auto-complete)))

(defun sanityinc/never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions
        (cons 'sanityinc/auto-complete-at-point
              (remove 'sanityinc/auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode
                log-edit-mode org-mode text-mode haml-mode
                git-commit-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js3-mode css-mode less-css-mode sql-mode
                sql-interactive-mode
                inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))

(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)


(provide 'init-auto-complete)

;; ********** ;;

;; restore the "desktop" -- do this as late as possible
(if first-time
    (progn
      (desktop-load-default)
      (desktop-read)))

;; indicate that this file has been read at least once
(setq first-time nil)

;; no need to debug anything now
(setq debug-on-error nil)

;; all done
(message "All done, %s%s" (user-login-name) ".")

