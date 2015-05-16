(require 'tramp)
(setq tramp-default-method "ssh")
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))

(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(global-hl-line-mode 1)

(set-face-background 'hl-line "silver")

; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
)

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
)

(define-key global-map (kbd "M-p") 'previous-blank-line)
(define-key global-map (kbd "M-n") 'next-blank-line)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)
(define-key global-map (kbd "C-1") 'find-file)
(define-key global-map (kbd "C-2") 'find-file-other-window)
(define-key global-map [C-tab] 'indent-region)

(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [(control ?=)] 'text-scale-increase)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key [(control ?_)] 'text-scale-decrease)
(global-set-key (kbd "M-RET") (lambda () (interactive) (hs-toggle-hiding)))
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

(setq scroll-step 3)
(setq ispell-program-name "/usr/local/bin/ispell")
(setq ring-bell-function 'ignore)
(cua-mode 0)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq mouse-yank-at-point t)
(blink-cursor-mode 0)
(load-theme 'tango t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(set-face-attribute 'default nil :height 130)
(global-set-key [(control h)] 'delete-backward-char)
(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defconst my-mode-line-buffer-identification
  (list
   '(:eval
     (let ((host-name
            (or (file-remote-p default-directory 'host)
                (system-name))))
       (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
           (substring host-name 0 (match-beginning 1))
         host-name)))
   ": %12b"))
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
;;(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(setq-default
 mode-line-buffer-identification
 my-mode-line-buffer-identification)
 
; (back-button-mode -1)

(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(setq-default c-basic-offset 4)
(setq c-default-style "linux"
          c-basic-offset 4)
; (setq mode-line-format ) 
; (setq-default header-line-format mode-line-format) ; Copy mode-line
; (setq-default mode-line-format nil) ; Remove mode-line

(fringe-mode 0)
(display-battery-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Enable line numbersdisplay-time-24hr-format t
(global-linum-mode 0)

(defface egoge-display-time
   '((((type x w32 mac))
      ;; #060525 is the background colour of my default face.
      (:foreground "black" :inherit bold))
     (((type tty))
      (:foreground "gray")))

   "Face used to display the time in the mode line.")
;(setq display-time-string-forms
;      '((propertize (concat " " 24-hours ":" minutes " ")
; 		    'face 'egoge-display-time)))

(display-time-mode 1)
(setq use-dialog-box nil)

(tooltip-mode -1)
(setq line-number-mode t)
(setq column-number-mode t)
(line-number-mode t)
(column-number-mode t)
(global-font-lock-mode t)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)
(transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq apropos-do-all t)
(setq backward-delete-char-untabify nil)
(fboundp 'tool-bar-mode)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

;; See http://bzg.fr/emacs-hide-mode-line.html

;; (defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode RET to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
;; (hidden-mode-line-mode 1)

;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "proxy.com:8080")
;;     ("https" . "proxy.com:8080")))

;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil
(defun my-mark-word (N)
  (interactive "p")
  (if (and 
       (not (eq last-command this-command))
       (not (eq last-command 'my-mark-word-backward)))
      (set-mark (point)))
  (forward-word N))


(defun my-mark-word-backward (N)
  (interactive "p")
  (if (and
       (not (eq last-command this-command))
       (not (eq last-command 'my-mark-word)))
      (set-mark (point)))
  (backward-word N))

(local-set-key (kbd "M-k") 'my-mark-word)

(local-set-key (kbd "M-j") 'my-mark-word-backward)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))
(global-font-lock-mode -1)



(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
;;(split-window-horizontally)

;;(toggle-frame-fullscreen)
(which-func-mode 1)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(load-file "~/.emacs.d/org-present.el")
(load-file "~/.emacs.d/qml-mode.el")
(require 'go-mode-autoloads)
(add-hook 'before-save-hook #'gofmt-before-save)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org ergoemacs-mode clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
