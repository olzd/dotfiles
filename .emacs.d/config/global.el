;; Taken (and adapted) from github.com/chrisdone/chrisdone-emacs/...


;;;;;; Requirements

(require 'uniquify)
;(require 'sgml-mode)


;;;;;; Functions

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (execute-kbd-macro [?\C-e S-home]))

(defun set-auto-saves ()
  "Put autosave files (ie #foo#) in one place, *not*
scaterred all over the file system!"
  (defvar autosave-dir
    (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
  
  (make-directory autosave-dir t)
  
  (defun auto-save-file-name-p (filename)
    (string-match "^#.*#$" (file-name-nondirectory filename)))

  (defun make-auto-save-file-name ()
    (concat autosave-dir
            (if buffer-file-name
                (concat "#" (file-name-nondirectory buffer-file-name) "#")
              (expand-file-name
               (concat "#%" (buffer-name) "#")))))

  (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
  (setq backup-directory-alist (list (cons "." backup-dir))))

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun github-ticket-open (&optional ticket)
  "Open the ticket number at point."
  (interactive)
  (let ((number (or ticket
                    (github-get-ticket))))
    (unless (string= number "")
      (browse-url (concat github-ticket-prefix number)))))

(defun github-get-ticket ()
  "Get the ticket number at point."
  (save-excursion
    (when (looking-at "#")
      (forward-char))
    (search-backward-regexp "[^0-9]" (line-beginning-position) t 1)
    (forward-char)
    (let* ((start (point))
           (number (progn (search-forward-regexp "[0-9]+" (line-end-position) t)
                          (buffer-substring-no-properties start
                                                          (point)))))
      number)))

(defun project-todo ()
  "Generate a TODO.org file from the project's files."
  (interactive)
  (let ((dir (or (when (boundp 'project-directory) project-directory)
                 (ido-read-directory-name "Project dir: " default-directory))))
    (find-file (concat dir "/TODO.org"))
    (erase-buffer)
    (insert (shell-command-to-string (concat "todo " dir)))
    (save-buffer)))

(defun reorder-buffer-list (pre-sort-list)
  "Re-order the buffer list."
  (let* ((sort-list (remove-if-not #'buffer-live-p pre-sort-list))
         (sort-len (length sort-list)))
    (mapc #'bury-buffer sort-list)
    (let* ((buffers (buffer-list))
           (buffers-len (length buffers)))
      (loop repeat (- buffers-len sort-len)
            for buf in buffers
            do (bury-buffer buf)))))

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d")))
    ad-do-it))


;;;;;; Global keybindings

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

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c g") 'gdb)

(global-set-key (kbd "s-u") 'winner-mode-undo)

(global-set-key (kbd "C-\\") 'goto-last-point)
(global-set-key (kbd "C-v") 'magit-switch-buffer)
(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-x") 'goto-last-change)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-x l") 'select-current-line)
(global-set-key (kbd "C-z") 'ido-switch-buffer)

(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)

(global-set-key [mouse-3] 'imenu)

;;;;;; Mode specific keybindings

(eval-after-load "markdown"
  '(define-key markdown-mode-map (kbd "M-;") 'markdown-blockquote-region))

(eval-after-load "sgml"
  '(define-key sgml-mode-map (kbd "/") nil))
(eval-after-load "c-mode"
  '(define-key c-mode-map (kbd "/") nil))
(eval-after-load "ag-mode"
  (progn
    '(define-key ag-mode-map (kbd "p") 'previous-error-no-select)
    '(define-key ag-mode-map (kbd "n") 'next-error-no-select)))


;;;;;; Disable default settings

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;;;;; Enable cool modes

(global-auto-complete-mode)

(ido-mode 1)
(evil-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(key-chord-mode 1)

(iswitchb-mode t)
(display-time-mode t)

(setq smex-save-file "~/.emacs.d/.smex-items")
(setq smex-auto-update 60)

(require 'speedbar)
(speedbar-add-supported-extension ".hs")

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files ".hi$\\|\\.o$")

(require 'whitespace)
;; (setq whitespace-style '(face lines-tail))
(setq whitespace-style nil)
(setq whitespace-line-column 80)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))
;; 
;; (package-initialize)

;; term-mode settings

(setq comint-input-ignoredups t)
(setq shell-completion-execonly nil)
(setq comint-prompt-read-only t)
(setq shell-completion '("~" "#" "%"))


;;;;;; Enable cool defaults

(line-number-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(set-auto-saves)


;;;;;; Default mode settings

(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'box)
(setq-default fill-column 80)

(setq c-default-style "k&r")
(setq c-basic-offset 4)

(setq gnus-button-url 'browse-url-generic)

(setq ido-ignore-files '("\\.hi$" "\\.o$" "\\.tags$" "^\\.ghci$"))
(setq ido-max-directory-size 200000)

(setq browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "DONE")
                          (sequence "TODO" "PASS")
                          (sequence "TODO" "DEFERRED"))
      org-priority-faces (quote ((49 . zenburn-red)
                                 (50 . zenburn-yellow)
                                 (51 . zenburn-green))))

(setq org-priority-faces (quote ((49 . sunburn-red)
                                 (50 . sunburn-yellow)
                                 (51 . sunburn-green))))


;;;;;; Global settings

(setq tab-width 2)
(setq scroll-step 1) ;; see scroll-conservatively ?

(fset 'yes-or-no-p 'y-or-n-p)

(setq require-final-newline t)

(global-font-lock-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(global-whitespace-mode t)

;; (setq custom-file (locate-user-emacs-file "config/custom.el")
;; (load custom-file)

;;;;;; Hooks

(add-hook 'text-mode-hook 'auto-fill-mode)


;;;;;; Auto-loads

; (load-theme 'monokai t)

(add-to-list 'default-frame-alist '(font . "Droid SansMono-10.5"))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'haskell-mode))
(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . haskell-core-mode))

(add-to-list 'auto-mode-alist (cons "\\.el\\'" 'emacs-lisp-mode))

(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))
(add-to-list 'auto-mode-alist (cons "\\.markdown\\'" 'markdown-mode))


;;;;;; Environment settings

(set-language-environment "UTF-8")


;;;;;; Uniquify

(setq uniquify-buffer-name-style (quote post-forward-angle-brackets))


;;;;;; Safe local variables

(custom-set-variables
 '(safe-local-variable-values
   (quote ((haskell-indent-spaces . 4)
           (haskell-indent-spaces . 2)
           (haskell-process-type . cabal-repl)
           (shm-lambda-indent-style . leftmost-parent)))))
 
