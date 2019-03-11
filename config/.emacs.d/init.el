;;;;
;;;; General Emacs configurations
;;;;
(package-initialize)                                       ; Commenting out and using targeted loads using use-package instead
(global-visual-line-mode t)                                ; Wrap text
(set-default-coding-systems 'utf-8)                        ; Set coding system to standard UTF-8. This should cover all characters needed.
(fset 'yes-or-no-p 'y-or-n-p)                              ; Require only y or n when a yes no question is asked
(setq show-paren-delay 0)                                  ; Highlight matching parens immediately without any delay
(show-paren-mode 1)                                        ; Highlight matching parens when cursor is on one of them
(line-number-mode t)                                       ; Show line number in the in the mode line
(column-number-mode t)                                     ; Show column number in the mode line
(add-hook 'prog-mode-hook (lambda ()
                            (modify-syntax-entry ?_ "w"))) ; Treat _ as part of a word. Useful for evil e.g. ciw.

;; Elisp mode setup
;; NOTE: i don't know what use-package block i can use here seen as elisp is native to emacs so just going to stick it in here...
(define-key lisp-mode-shared-map (kbd "C-c C-f") 'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-c C-b") 'eval-buffer)
(define-key lisp-mode-shared-map (kbd "C-c C-r") 'eval-region)


;; Need this for work firewall
(if (file-exists-p "~/.emacs.d/.emacs_proxy")
   (load "~/.emacs.d/.emacs_proxy"))    

;; Save all the backup files in a single directory instead of the directory where the original files reside.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Removes buffer names from the buffer name history after they have been killed.
(add-hook 'kill-buffer-hook
   (lambda ()
    (setq buffer-name-history
          (delete (buffer-name) buffer-name-history)
    )
   )
)


;;;;
;;;; Package management
;;;;
;; Define package repositories and load-path
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'load-path "~/.emacs.d/elpa")

;; Load use-package manually so that all other packages can be managed by use-package
(when (not (require 'use-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

;; Evil mode setup
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil) ; Setting this before evil loads so that TAB works as expected in org-mode
  :config
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "g"  'magit-status))
  (use-package key-chord ; This is needed to bind jk to escape insert mode
    :ensure t
    :config
    (use-package key-seq
      :ensure t)
    (key-chord-mode 1))
  (evil-mode 1)
  (key-seq-define evil-insert-state-map "jk" 'evil-normal-state)
  (evil-global-set-key 'motion "H" 'evil-beginning-of-visual-line)
  (evil-global-set-key 'motion "L" 'evil-end-of-visual-line)
  (evil-global-set-key 'motion " " 'evil-toggle-fold))

;; Solarized color theme setup
(use-package color-theme-solarized
  :ensure t
  :config
  (use-package color-theme :ensure t)
  (set-terminal-parameter nil 'background-mode 'dark)
  (load-theme 'solarized t))

;; Org-mode setup
(use-package org
  :config
  (use-package org-bullets
    :ensure t)
  (load "~/.emacs.d/org_mode_config.el"))

;; Magit setup
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; linum setup
(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook 'linum-relative-mode))

;; Help setup
(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  ("C-c h o" . helm-occur)
  :config
  (use-package helm-config)
  (set-face-attribute 'helm-selection nil :background "green" :foreground "black"))

;; ESS setup
;; Becuase of the way this package is named have to manually install it.
;(when (not (require 'ess nil 'noerror))
;  (package-refresh-contents)
;  (package-install 'ess)
;  (require 'ess))
;(use-package ess-site
;  ;:ensure t ; Can't use this because package name in elpa is ess and it doesn't match ess-site
;  :config
;  (ess-toggle-underscore nil)                         ; I use underscore in my var names so turning off this behaviour
;  (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
;  (setq ess-default-style 'RStudio))                   ; I like the RStudio indentation of 2 spaces

;; Flyspell setup
(use-package flyspell
  :ensure t
  :config
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1)))))

;; Origami setup
(use-package origami
  :ensure t
  :diminish origami-mode
  :config
  (global-origami-mode t)
  (add-to-list 'origami-parser-alist '(R-mode . origami-c-style-parser))
  (add-hook 'R-mode-hook (lambda () (interactive)
                           (call-interactively 'origami-close-all-nodes)) t))

;; Use cperl-mode instead of perl-mode
(use-package cperl-mode
  :ensure t
  :config
  ;; comprehensively switch from perl-mode to cperl-mode
  (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
  
  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  (setq cperl-hairy t)          ; Turns on most of cperl options.
  (setq cperl-invalid-face nil) ; Turns off underscore for trailing whitespace.
  (setq cperl-indent-level 4))
 

;; Load any local packages specific to this machine. Useful to loading work/home specific stuff.
(if (file-exists-p "~/.emacs.d/local_init.el")
   (load "~/.emacs.d/local_init.el"))    


;;;;
;;;; Further customisation
;;;;

;; Buffer manipulation
;; These were stolen from Magnar Sveen https://github.com/magnars/.emacs.d
(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

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

(defun rotate-windows ()
  "Rotate your windows

   Modified from Magnar's version to put focus back to the buffer that was in focus to begin with"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (let ((this-win-buffer (window-buffer)))
           (setq i 1)
           (setq numWindows (count-windows))
           (while  (< i numWindows)
             (let* (
                    (w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2))
                    )
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))
           (select-window (get-buffer-window this-win-buffer))))))

(global-set-key (kbd "C-c s") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-c t") 'toggle-window-split)
(global-set-key (kbd "C-c r") 'rotate-windows)

;; Window manipulation
;; Took this from https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
