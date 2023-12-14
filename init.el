;;   EEEEE  MM      MM      AA       CCCC  SSSS
;;   EE     MMMM  MMMM     AAAA     CCC   SSS
;;   EEEEE  MM MMMM MM    AA  AA    CC     SSSS
;;   EE     MM  MM  MM   AAAAAAAA   CCC      SSS
;;   EEEEE  MM      MM  AA      AA   CCCC  SSSS
;; [=============================================]


;;                   PACKAGES
;;                  [========]

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use Package
(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/elpa/use-package-20230203.2004")
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Auto Update
;; (use-package auto-package
;;   :defer 10
;;   :config
;;   ;; Delete residual old versions
;;   (setq auto-package-update-delete-old-versions t)
;;   ;; Do not bother me when updates have taken place.
;;   (setq auto-package-update-hide-results t)
;;   ;; Update installed packages at startup if there is an update pending.
;;   (auto-package-update-maybe))

;; Projectile
(use-package projectile
  :config

  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Magit
(use-package magit)

;; Recentf
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "~/.config/emacs/var/recentf"))
  :init
  (recentf-mode 1)
  (global-set-key (kbd "C-c r") 'recentf-open-files))

;; Counsel
(use-package counsel
  :init
  (counsel-mode 1))

;; Ivy/Swiper
(use-package swiper)
(use-package ivy
  :after
  (counsel)
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-S-s") 'isearch-forward)
  (setq ivy-initial-inputs-alist '((counsel-minor . "^+")
                                  (counsel-package . "^+")
                                  (counsel-org-capture . "^")
                                  (counsel-M-x . "^")
                                  (counsel-describe-symbol . "^")
                                  (org-refile . "^")
                                  (org-agenda-refile . "^")
                                  (org-capture-refile . "^")
                                  (Man-completion-table . "^")
                                  (woman . "^")))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :init
  (ivy-mode 1))

;; Ivy-Prescient
;; (use-package ivy-prescient
;;   :after counsel
;;   :config
;;   (ivy-prescient-mode 1))

;; Hydra
(use-package hydra)

;; Helpful
;; Relpace normal C-h binds with more helpful ones
(use-package helpful
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;; More Helpful Keys
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)

;; Beacon
;; Highlights cursor so you don't get lost.
(use-package beacon
  :init
  (setq beacon-blink-when-point-moves t)
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-window-scrolls t)
  (setq beacon-blink-when-buffer-changes t)
  (setq beacon-blink-when-focused t)
  (beacon-mode 1))

;; Minimap
(use-package minimap
  :config
  (setq minimap-mode t)
  (setq minimap-update-delay 0)
  (setq minimap-window-location 'right)
  ;; (setq minimap-active-region-background ((t (:extend nil))))
  :init
  (minimap-mode 0))

;; WS-Butler
;; Cleans up whitespace on the ends of lines.
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Comment-DWIM
;; Allows for commenting out reigons and lines with a keypress. (M-;)
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)
        (:map org-mode-map
              ("M-;" . org-comment-dwim-2))))

;; Which-Key
;; Shows keybind in command minibuffer menu.
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))

;; Smartparens
;; Automatic parenthesis placement
(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t))
  (global-set-key (kbd "M-C-f") 'sp-forward-sexp)
  (global-set-key (kbd "M-C-b") 'sp-backward-sexp))

;; Undo Fu
;; More understandable undo system for a pea brain like me
(use-package undo-fu
  :init
  (global-set-key (kbd "M-u")   'undo-fu-only-undo)
  (global-set-key (kbd "M-r") 'undo-fu-only-redo)
  (global-set-key (kbd "M-S-r") 'undo-fu-only-redo)
  (setq undo-limit 67108864)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960))

;; Undo Fu Session
;; Keeps undos from undo-fu even when emacs is closed
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode))

;; Flycheck
(use-package flycheck
  :bind
  (("C-c f" . flycheck-mode))
  ;; :init
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
)

;; Rainbow Delimiters
;; Highlights Corresponding Brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Keep .config/emacs clean
(use-package no-littering)

;; Emojify
(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Elcord
;; Discord rich prescence for Emacs
(use-package elcord
  :init
  (elcord-mode))

;; Neotree
(use-package neotree
  :config
  (setq neo-theme 'nerd))

;; Doom modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; VTerm
(use-package vterm
  :bind
  (("M-RET" . vterm)))

;; Custom Dashboard
(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  ;; (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  ;; (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  ;; (setq dashboard-set-heading-icons t)
  ;; (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-item-shortcuts
        '((recents   . "r")
          (bookmarks . "b")
          (projects  . "p")
          (agenda    . "a")
          (registers . "e")))
  :init
  (dashboard-setup-startup-hook))

(use-package elfeed-dashboard
  :config
  (setq elfeed-dashboard-file "~/.config/emacs/elfeed/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links)
  :init
  (load "~/.config/emacs/elfeed/feeds.el"))

;; Dirvish
;; an improved version of the Emacs inbuilt package Dired.
(use-package dirvish
  :init
  (dirvish-override-dired-mode 1))

;; ibuffer
(use-package ibuffer
  :config
  (setq ibuffer-save-with-custom nil
        ibuffer-saved-filter-groups
        '(("default"
           ("pdf" (mode . pdf-view-mode))
           ("code" (and (or (derived-mode . prog-mode) (mode . yaml-mode)) (not (name . "^\\*scratch\\*$"))))
           ("dired" (or (mode . dired-mode) (mode . dired-mode))
           ("special" (and (name . "^\*") (not (name . "^\\*scratch\\*$"))))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

;; Impatient Mode
;; emacs live preview
(use-package impatient-mode
  :config
  (defun impatient-start ()
    (interactive)
    (httpd-start)
    (impatient-mode 1)
    (browse-url "http://localhost:8080/imp/"))
  (global-set-key (kbd "C-c C-l") 'impatient-start)
  (global-set-key (kbd "C-c C-L") 'impatient-mode))


;;             ORG MODE
;;            [========]

;; org-bullets
;; Replace *s with utf-8 bullets
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-hide-leading-stars t)

;; Syntax highlighting
(setq org-src-fontify-natively t)

;; toc-org
;; Table of contents for org
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook '^org-mode-hook 'toc-org-enable))

;; Org agenda file
(setq org-agenda-files "~/agenda.org")

;; (custom-set-faces
;;  '(org-level-4 :inherit outline-4 :height 1.1)
;;  '(org-level-3 :inherit outline-3 :height 1.25)
;;  '(org-level-2 :inherit outline-2 :height 1.5)
;;  '(org-level-1 :inherit outline-1 :height 1.75)
;;  '(org-document-title :height 2.0 :underline nil))

;; Prettify Symbols
(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        '(("[ ]" . ?󰄱)
          ("[X]" . ?󰱒)
          ("#+DATE:" . ?󰃮)
          ("#+begin_src" . ?)
          ("#+end_src" . ?)
          ("#+begin_example" .)
          ("#+end_example" .)))
  (prettify-symbols-mode 1))

;;             BASIC CONFIGURATAION
;;            [====================]

;; Load Theme Based On Current Host
;; Load Catppuccin on my desktop and Wombat on my Thinkpad
(if (string= (system-name) "littlefella")
    (progn
      (add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
      (setq catppuccin-flavor 'macchiato)
      (load-theme 'catppuccin t))
  (load-theme 'wombat))

;; Font
(custom-set-faces
 '(default ((t (:family "JetBrainsMono Nerd Font" :foundry "JB" :slant normal
                :weight normal :height 113 :width expanded)))))

;; Remove Black Bars From Emacs Window
(set-fringe-mode 0)

;; Remove all GUI!
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)

;; Add 80 Column Line to Clean up Code
(display-fill-column-indicator-mode 1)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Whitespace Mode
;; Highlights lines over 80 characters.
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; User Configuration
(setq user-mail-address "nhe@air.net.au")

;; Remove Scratch Message
(setq initial-scratch-message nil)

;; Change C-a Behaviour
;; C-a now sends the cursor to the first non whitespace character on a line
;; (consecutive C-a will send to the true beggining of line)
(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

;; Double Check Before Quit
(setq confirm-kill-proccesses t)


;; Refresh File When File Changes Outside of Emacs
(global-auto-revert-mode 1)

;; Refresh Buffers Like Dired When File Changes Outside of Emacs
(setq global-auto-revert-non-file-buffers t)

;; UTF-8 Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Line Numbers
(global-display-line-numbers-mode 1)

;; Line Numbers in Some Modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Increase Wrap Column to fit modern standards
(setq-default fill-column 80)

;; Disable Double Space After Sentences
(setq-default sentence-end-double-space nil)

;; Add Trailing Newline to end of files
(setq require-final-newline t)

;; Line Mode (Highlight current line
;; (could uses some theme to make it more subtle))
(hl-line-mode -1)

;; Tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Tab Indents Before Completion
(setq-default tab-always-indent 'complete)

;; Auto Indent on Newline
(electric-indent-mode 1)

;; Unbind Manual Newline and Indent
(global-unset-key ( kbd "C-j"))

;; Toggle Auto Indentations
(defun turn-on-electric-indent-mode ()
  "Turns on electric-indent-mode"
  (interactive)
  (electric-indent-mode 1))

(defun turn-off-electric-indent-mode ()
  "Turns off electric-indent-mode"
  (interactive)
  (electric-indent-mode -1))

(add-hook 'prog-mode-hook 'turn-on-electric-indent-mode)

;; Improve Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Cursor Configurations
(setq cursor-type 'box)
(blink-cursor-mode 1)

;; Remove Warning on Large Files
(setq large-file-warning-threshold nil)

;; Remove Warning on Advised Functions
(setq ad-redefinition-action 'accept)

;;             KEYBOARD MODIFICATIONS
;;            [======================]

;; Bind M-u and M-l to C-c u and C-c l because they get in the way and I never
;; use them
(global-set-key (kbd "C-c u") 'upcase-word)
(global-set-key (kbd "C-c l") 'downcase-word)

;; Bind M-p to previous paragraph to match C-Up
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Bind M-n to previous paragraph to match C-Down
(global-set-key (kbd "M-n") 'forward-paragraph)

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Bind C-S-j to Goto Line
(global-set-key (kbd "C-S-j") 'goto-line)

;; Unbind C-z because C-x C-z does the same thing
(global-unset-key (kbd "C-z"))

;; Bind C-r to repeat so that I don't have to C-x z
(global-set-key (kbd "C-r") 'repeat)

;; Zoom Keys
;; C-z i to zoom in and C-z o to zoom out
(defhydra hydra-zoom (global-map "C-z")
  "zoom"
  ("i" text-scale-increase "in")
  ("o" text-scale-decrease "out"))

(global-unset-key (kbd "M-t"))
(global-set-key (kbd "C-S-t") #'transpose-words)

;; Toggle IDE-like elements
(defhydra hydra-toggle (global-map "M-t")
  "toggle elements"
  ("f" neotree-toggle "toggle neotree")
  ("m" minimap-mode "toggle minimap")
  ("t" vterm-toggle "toggle terminal")
  ("p" smartparens-mode "toggle auto parenthesis"))

;; Dictionary Definition of Current Word
(global-set-key (kbd "C-h d") #'dictionary-lookup-definition)
(global-set-key (kbd "C-h C-d") #'apropos-documentation)

;; Bind M-o to switch frame becuase C-x o is slow
(global-set-key (kbd "M-o") 'other-window)

;; Change list-buffers to ibuffer because ibuffer is cooler B)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;             BACKUP CONFIGURATAION
;;            [=====================]

;; Backup Directory
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))

;; Only Keep 500 Backups Of A File
(setq kept-new-versions 500)

;; Clean Out Old Backups
(setq delete-old-versions t)

;; Backups Work With Symlinks
(setq backup-by-copying t)

;; Add Version Numbers to Backups
(setq version-control t)

;; Make Backup Every Save
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;;                      MISC
;;                    [======]

;; Replace yes-or-no-p with a shorter y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; TheThe
(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)

;; This regexp is not perfect
;; but is fairly good over all:
  (if 2(re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))


;; Bind 'the-the' to  C-c
(global-set-key "\C-c\\" 'the-the)

(defun set-elisp-mode-settings ()
  "Sets my keybinds to use elisp evaluation commands"
  (local-set-key (kbd "C-c C-r") 'eval-region)
  (local-set-key (kbd "C-c C-b") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook 'set-elisp-mode-settings)

;; Remove Bothersome Compiler Warnings
(setq comp-async-report-warnings-errors nil)
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; Time Emacs Startup
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Custom File to clean up init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
