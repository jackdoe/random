(require 'tramp)
    (setq tramp-default-method "ssh")
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

; (set-face-attribute 'default nil :font "Liberation Mono-10")

(cua-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq mouse-yank-at-point t)
(blink-cursor-mode 0)
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(set-face-attribute 'default nil :height 110)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(setq-default
 mode-line-buffer-identification
 my-mode-line-buffer-identification)
 
; (back-button-mode -1)
(load-theme 'wombat t)
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
