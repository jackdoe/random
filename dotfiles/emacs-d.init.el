(require 'tramp)
;;(setq mouse-autoselect-window t
;;      focus-follows-mouse t)

(setq tramp-default-method "ssh")
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/lib/dart/bin"))
(setq exec-path (append exec-path '("/usr/lib/dart/bin")))
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(global-hl-line-mode 1)

; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n"))

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1))
(set-face-attribute 'default nil :family "terminus" :height 130 :weight 'bold)
(set-face-background 'hl-line "#dadada")
(define-key global-map (kbd "M-p") 'previous-blank-line)
(define-key global-map (kbd "M-n") 'next-blank-line)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
;;(define-key global-map [M-return] 'hs-toggle-hiding)
(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)
(define-key global-map (kbd "<f5>") 'switch-to-buffer)
(define-key global-map (kbd "<f6>") 'find-file)
(define-key global-map (kbd "<f1>") 'save-buffer)
(define-key global-map (kbd "<f2>") 'kill-buffer)
(define-key global-map (kbd "<f10>") 'revert-buffer)
(define-key global-map (kbd "<f9>") 'replace-string)

(define-key global-map [C-tab] 'indent-region)

(global-set-key [(control ?+)] 'text-scale-increase)
(global-set-key [(control ?=)] 'text-scale-increase)
(global-set-key [(control ?-)] 'text-scale-decrease)
(global-set-key [(control ?_)] 'text-scale-decrease)

(global-set-key (kbd "s-C-]") 'shrink-window-horizontally)
(global-set-key (kbd "s-ESC") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-{") 'shrink-window)
(global-set-key (kbd "s-C-}") 'enlarge-window)
;;(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

(global-set-key (kbd "C-0") (lambda () (interactive) (other-frame)))

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode 1)

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
;;(set-face-attribute 'default nil :height 110)
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

(defun system-out-println ()
  (interactive)
  (save-excursion
    (insert "System.out.println();")))

(package-initialize)


(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(setq-default c-basic-offset 4)
(setq c-default-style "linux"
          c-basic-offset 4)

(fringe-mode 0)
(display-battery-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

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

(which-func-mode 1)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)


;; enable recent files mode.
(recentf-mode t)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren))
(setq cperl-indent-parens-as-block t)
(setq perl-indent-parens-as-block t)
(setq java-indent-parens-as-block t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (prettier-js rjsx-mode web-mode company-go projectile ido-vertical-mode neotree godoctor ascii exwm-surf exwm eglot lsp-mode dart-mode elixir-mode go-mode multiple-cursors)))
 '(require-final-newline nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(defun my-go-mode-hook ()
  ; (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ; Go oracle
  ; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(setq dart-format-on-save t)
;;(add-to-list 'eglot-server-programs '(dart-mode . ("dart_language_server")))
(defvar dart-analyzer "/usr/lib/dart/bin/dartanalyzer")
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(add-hook 'dart-mode-hook 'lsp)

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(setq lsp-auto-guess-root t)
 (require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)



(setq exwm-workspace-show-all-buffers t)
;; ;; ;;add keys to disable and enable the mouse
;; ;; ;;xinput set-prop 12 "Device Enabled" 0
;; ;; (set-background-color "black")

;; ;; (set-foreground-color "white")

;; ;; (set-face-attribute 'region nil :background "#666")
;; ;; (set-cursor-color "#ffffff")
;; ;; (require 'paren)
;; ;; (set-face-background 'show-paren-match "#888")
;; ;; (set-face-foreground 'show-paren-match (face-foreground 'default))


;;(set-face-attribute 'show-paren-match nil :weight 'normal)

(setq-default line-spacing 1)





(server-start)




(global-unset-key (kbd "C-t"))

(require 'exwm)
(require 'exwm-config)
(require 'exwm-surf)

(defvar elmord/exwm-buffer-name-limit 50)

(defun elmord/exwm-compute-buffer-name ()
  (let ((class (or exwm-class-name ""))
        (title (or exwm-title "")))
    (cond
     ((and (member class '("Firefox" "Firefox-esr" "Icedove"))
           (string-match "\\`\\(.*\\)\\( - [^-]*\\)\\'" title))
      (concat class ": " (match-string 1 title)))
     ((member class '("Telegram" "TelegramDesktop"))
      (if (equal title "") "Telegram"e title))
     ((equal class "skypeforlinux") "Skype")
     (t (concat class ": " title)))))

(defun elmord/exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer
   (truncate-string-to-width
    (elmord/exwm-compute-buffer-name)
    elmord/exwm-buffer-name-limit
    nil  ; start
    nil  ; padding
    "…")
    ))

(add-hook 'exwm-update-class-hook 'elmord/exwm-update-buffer-name)
(add-hook 'exwm-update-title-hook 'elmord/exwm-update-buffer-name)

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-h] . [backspace])
        ([?\C-d] . [delete])
        ([?\C-.] . [?\C-w])
        ([?\C-}] . [C-tab])
        ([?\C-{] . [C-S-tab])
        ([?\C-k] . [S-end delete])

        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ([?\C-g] . [escape])

        ;; always open new window on c-t, ignore the transpose thing
        ;; ([?\C-t] . [?\C-n])
        ;; search
        ([?\C-s] . [?\C-f])))

(add-hook
 'exwm-manage-finish-hook
 (defun init-exwm/set-xterm-simulation-keys ()
   (when (or
          (and exwm-class-name (string= exwm-class-name "XTerm"))
          (and exwm-class-name (string= exwm-class-name "URxvt")))
     (exwm-input-set-local-simulation-keys
      '(([?\M-w] . [?\C-,])
        ([?\C-y] . [?\C-.])
        ([?\C-h] . [backspace])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [S-prior])
        ([?\C-v] . [S-next])

        ([?\C-c] . [?\C-c]))))
   ))
 

(exwm-input-set-key (kbd "s-d") 'rename-buffer)
(exwm-input-set-key (kbd "s-D") 'exwm-reset)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-o") 'other-window)
(exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)
(setq exwm-workspace-number 4)

(define-key exwm-mode-map (kbd "C-x C-x")
  (lambda () (interactive) (exwm-input--fake-key ?\C-x)))

(define-key exwm-mode-map (kbd "C-c C-c")
(lambda () (interactive) (exwm-input--fake-key ?\C-c)))

(setq exwm-input-global-keys 
      `(
        ([?\s-d] . exwm-reset)
        ([?\s-m] . exwm-workspace-move-window)
        ([?\s-o] . other-window)
        ([?\s-w] . exwm-workspace-switch)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 5))
        ([?\s-r] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))
        ([print] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/shot-whole")))
        ([s-return] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/urxvt")))))


(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(defun display/brighter ()
  (interactive)
  (start-process-shell-command "xbacklight" nil "xbacklight -inc 5"))
(defun display/darker()
  (interactive)
  (start-process-shell-command "xbacklight" nil "xbacklight -dec 5"))

(defun mouse/toggle ()
  (interactive)
  (start-process-shell-command "xinput" nil "/usr/bin/toggle-mouse.sh"))


(defun audio/up ()
  (interactive)
  (start-process-shell-command "pactl" nil "pactl -- set-sink-volume 0 +5%"))
(defun audio/down()
  (interactive)
  (start-process-shell-command "pactl" nil "pactl -- set-sink-volume 0 -5%"))
(defun audio/mute()
  (interactive)
  (start-process-shell-command "pactl" nil "pactl set-sink-mute 0 toggle"))

;;(set-frame-parameter nil 'fullscreen 'fullboth)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'display/brighter)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'display/darker)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'audio/up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'audio/down)
(exwm-input-set-key (kbd "<XF86AudioMute>") 'audio/mute)
(exwm-input-set-key (kbd "<XF86Search>") 'mouse/toggle)
(exwm-input-set-key (kbd "s-C-]") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-ESC") 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-C-{") 'shrink-window)
(exwm-input-set-key (kbd "s-C-}") 'enlarge-window)


(exwm-enable)
(exwm-config-ido)
(exwm-config-misc)
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "eDP1" 2 "eDP1" 3 "eDP1" 4 "eDP1" 5 "DP1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP1 --right-of eDP1 --mode 2560x1440")))

(setq use-dialog-box nil)

;; dont put in m-backspace in kill ring
(defun delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "<M-backspace>") 'delete-word)

(set-face-attribute 'default nil :family "dejavu sans mono" :height 120 :weight 'normal)
(require 'elixir-mode)
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(require 'godoctor)
(setq ido-enable-flex-matching t)
(setq mode-require-final-newline nil)
(setq require-final-newline nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-.")
                (lambda () (interactive "")
                  (switch-to-buffer (other-buffer (current-buffer) t))))

(require 's)
;;(require 'company)
(add-to-list 'load-path "~/.emacs.d/alchemist.el/")
(require 'alchemist)
;;(require 'company-go)

(add-hook 'elixir-mode-hook
      (lambda ()
        (company-mode)))

(global-company-mode 0)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;;(define-key global-map (kbd "s-g") 'projetile-grep)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(exwm-randr-enable)
;;(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(package-require 'tern)
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
