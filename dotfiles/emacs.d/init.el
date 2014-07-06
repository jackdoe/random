(require 'tramp)
    (setq tramp-default-method "ssh")
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))
(setq ispell-program-name "/usr/local/bin/ispell")
(setq ring-bell-function 'ignore)
;; (set-face-attribute 'default nil :font "Liberation Mono-10")
;; (cua-mode 1)
(cua-mode 0)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq mouse-yank-at-point t)
(blink-cursor-mode 0)
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'wombat t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-file "~/.emacs.d/lisp/go-mode.el")
(set-face-attribute 'default nil :height 110)
(global-set-key [(control h)] 'delete-backward-char)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(tramp-use-ssh-controlmaster-options nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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

(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [(control ?=)] 'text-scale-increase)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key [(control ?_)] 'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

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
