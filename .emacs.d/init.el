;; Taken (and adapted) from github.com/chrisdone/chrisdone-emacs/...

;; Standard libraries needed

(require 'cl)


;; Packages and configs to load

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; (defvar packages
;;   '(color-theme
;;     ;god-mode
;;     haskell-mode
;;     smex
;;     ;git-modes
;;     magit
;;     notify
;;     goto-last-change
;;     markdown-mode
;;     ace-jump-mode
;;     dash
;;     elisp-slime-nav
;;     column-marker
;;     align-by-current-symbol
;;     ;rainbow-mode
;;     ag
;;     ;w3m
;;     goto-last-point
;;     ;github-url
;;     ))

(defvar local-packages
  '(github-urls))

(defvar configs
  '("global"
    ;"god"
    "haskell"
    ;"erlang"
    "magit"  
    "email"
    "lisp"
    "python"
    ;"elget"
    "evil"
    "w3m"
    "custom"))

(setq font-lock-maximum-decoration 4)

;; Load packages

;; (add-to-list 'load-path "~/.emacs.d/el-get/git-modes")
;; (add-to-list 'load-path "~/.emacs.d/el-get/emacs-w3m")

(loop for name in local-packages
      do (progn (unless (fboundp name)
                  (add-to-list 'load-path
                               (concat (file-name-directory (or load-file-name
                                                                  (buffer-file-name)))
                                        "el-get-user/"
                                        (symbol-name name)))
                  (require name))))
                                                             
;; (loop for name in packages
;;       do (progn (unless (fboundp name)
;;                   (add-to-list 'load-path
;;                                (concat (file-name-directory (or load-file-name
;;                                                                 (buffer-file-name)))
;;                                        "el-get/"
;;                                        (symbol-name name)))
;;                   (require name))))

;(add-to-list 'load-path
;             (concat (file-name-directory (or load-file-name
;                                              (buffer-file-name)))
;                     "el-get/"
;                     "structured-haskell-mode/elisp"))
;(add-to-list 'load-path "~/.emacs.d/el-get/structured-haskell-mode/elisp")
(add-to-list 'load-path "~/.cabal/bin/structured-haskell-mode")

(require 'shm)
(require 'shm-case-split)
(require 'w3m-haddock)


;; Global/standard Emacs configuration

(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "config/"
                       name ".el")))


;; Mode initializations

;;(auto-complete-mode)
(smex-initialize)
;(god-mode)
(structured-haskell-mode)
;(goto-last-point-mode)
(turn-on-haskell-simple-indent)
(load "haskell-mode-autoloads.el")

;; End of init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "9c26d896b2668f212f39f5b0206c5e3f0ac301611ced8a6f74afe4ee9c7e6311" default)))
 '(global-auto-complete-mode t)
 '(haskell-indent-spaces 2)
 '(haskell-interactive-mode-do-fast-keys t)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-mode-hook (quote (capitalized-words-mode turn-on-haskell-indentation turn-on-haskell-doc-mode capitalized-words-mode imenu-add-menubar-index structured-haskell-mode)))
 '(haskell-notify-p t)
 '(haskell-process-args-ghci (quote nil))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(safe-local-variable-values (quote ((haskell-indent-spaces . 4) (haskell-indent-spaces . 2) (haskell-process-type . cabal-repl) (shm-lambda-indent-style . leftmost-parent))))
 '(send-mail-function (quote smtpmail-send-it))
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-program-name "structured-haskell-mode")
 '(shm-use-hdevtools t)
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
