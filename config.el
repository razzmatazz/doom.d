;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!
(setq load-prefer-newer t)

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Saulius Menkevičius"
      user-mail-address "sauliusmenkevicius@fastmail.com")

(setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 17))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-manegarm)
(setq doom-theme 'tango)
(setq doom-theme 'zenburn)


;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;
;; system config
;;
(when IS-MAC
  (add-to-list 'exec-path "/usr/local/share/dotnet")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path (concat (getenv "HOME") "/.dotnet/tools"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/bin/google-cloud-sdk/bin"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))

  (setenv "LANG" "en_US.UTF-8")

  (setenv "PATH"
          (concat "/usr/local/share/dotnet" ":"
                  "/usr/local/bin" ":"
                  (concat (getenv "HOME") "/.dotnet/tools") ":"
                  (concat (getenv "HOME") "/bin/google-cloud-sdk/bin") ":"
                  (concat (getenv "HOME") "/.cargo/bin") ":"
                  (getenv "PATH"))))

;;
;; disable stuff from the default doom config
;;
(after! smartparens
  (smartparens-global-mode -1))

(after! evil-snipe
  (evil-snipe-mode -1))

(remove-hook 'tty-setup-hook 'doom-init-clipboard-in-tty-emacs-h)

;;
;; evil overrides
;;
(evil-set-initial-state 'shell-mode 'normal)

;; This is because C-i is bound to better-jumper-jump-forward, and in TTY emacs, the tab key triggers C-i.
;; https://github.com/hlissner/doom-emacs/issues/1367
(setq evil-want-C-i-jump nil)

;(remove-hook 'text-mode-hook #'evil-normal-state)

;;
;; org mode setup
;;
(setq org-directory "~/Dropbox/org/")

(after! org
  (setq org-startup-indented nil)
  (setq org-agenda-files '("~/Dropbox/org/journal.org"
                           "~/Dropbox/org/personal.org"
                           "~/Dropbox/org/printlog-mgmt.org"
                           "~/Dropbox/org/printlog-projects.org"
                           ))
; (setq org-icalendar-combined-agenda-file "~/Dropbox/Public/org-calendar.ics")
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-agenda-default-appointment-duration 60))

;;
;; menu & mode line
;;
(menu-bar-mode -1)
(display-time)
(remove-hook 'shell-mode-hook #'hide-mode-line-mode)

;;
;; misc
;;

(setq completion-ignore-case t)

(setq dabbrev-case-fold-search 'case-fold-search)
(setq dabbrev-case-replace nil)

(after! company
  (setq company-idle-delay 0.2))

(add-hook 'compilation-mode-hook #'visual-line-mode)
(add-hook 'prodigy-view-mode-hook #'visual-line-mode)

;;
;; mail
;;
(when IS-LINUX
  (add-load-path! "/usr/share/emacs/site-lisp/mu4e"))

(when IS-MAC
  (add-load-path! "/usr/local/share/emacs/site-lisp/mu/mu4e"))

(after! mu4e
  (setq! mu4e-maildir (expand-file-name "~/Maildir") ; the rest of the mu4e folders are RELATIVE to this one
         mu4e-headers-include-related nil
         mu4e-get-mail-command "mbsync -a"
         mu4e-index-update-in-background t
         mu4e-compose-signature-auto-include t
         mu4e-use-fancy-chars t
         mu4e-view-show-addresses t
         mu4e-view-show-images t
         mu4e-compose-format-flowed t
         mu4e-change-filenames-when-moving t ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
         mu4e-maildir-shortcuts
         '( ("/Inbox" . ?i)
            ("/Archive" . ?a)
            ("/Drafts" . ?d)
            ("/Deleted Items" . ?t)
            ("/Sent Items" . ?s))

         ;; Message Formatting and sending
         message-send-mail-function 'smtpmail-send-it
         message-citation-line-function 'message-insert-formatted-citation-line
         message-kill-buffer-on-exit t
         ))

;;
;; coding
;;
(setq lsp-eldoc-render-all t)
(setq lsp-auto-execute-action nil)
(setq lsp-ui-sideline-show-code-actions nil) ;; this can get slow for omnisharp-roslyn
(setq lsp-signature-auto-activate nil) ;; this crashes on omnisharp-roslyn or causes erratic UI flashes

;;
;; language: rust
;;
(setq rustic-lsp-server 'rust-analyzer)

;;
;; language: C#
;;

;; use local server version (at least for now, while developing)
(setq lsp-csharp-server-path (expand-file-name "~/src/omnisharp-server-local/run"))

(after! csharp-mode
  (defun sm-csharp-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4))

  (add-hook 'csharp-mode-hook 'sm-csharp-mode-setup t)

  (map! (:map csharp-mode-map
         (:leader
          (:prefix "c"
           (:prefix "t"
            :desc "Run test at point" "p" #'lsp-csharp-run-test-at-point
            :desc "Run all tests in buffer" "b" #'lsp-csharp-run-all-tests-in-buffer))))))

(after! omnisharp
  (setq omnisharp-expected-server-version "1.34.14")

  (defun sm-csharp-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4))

  (map! (:map omnisharp-mode-map
          :nm "C-]" #'omnisharp-go-to-definition
          :localleader
          (:prefix "r"
            "m"  #'omnisharp-rename
            "r"  #'omnisharp-run-code-action-refactoring)
          (:prefix "t"
            "p" #'omnisharp-unit-test-at-point
            "t" #'omnisharp-unit-test-last
            "b" #'omnisharp-unit-test-buffer)))

  ; temporary hack
  (defun +csharp-cleanup-omnisharp-server-h ()
    t))

;;
;; language: F#
;;

(after! fsharp-mode
  (defun sm-fsharp-mode-setup ()
    (fsharp-mode-indent-smie-setup))

  (map! (:map fsharp-mode-map
          "<backspace>" #'doom--backward-delete-whitespace-to-column))

  (add-hook 'fsharp-mode-hook 'sm-fsharp-mode-setup t))

;;
;; language: web/html, pug, etc.
;;
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))

(after! pug-mode
  (add-hook 'pug-mode-hook '(lambda ()
                              (setq evil-shift-width 4)
                              (setq pug-tab-width 2))))

;;
;; custom functions
;;
(defun sm-toggle-line-numbers ()
  "doom/toggle-line-numbers uses 'relative style when toggling, I don't want that"
  (interactive)
  (setq display-line-numbers (if display-line-numbers nil t)))


(defun sm-hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIXand DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun sm-move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun sm-move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun sm-csharp-alt-lsp-server ()
  (interactive)
  (setq lsp-csharp-server-path
        (expand-file-name
         "~/src/csharp-language-server/src/CSharpLanguageServer/bin/Debug/netcoreapp3.1/CSharpLanguageServer")))

(defun sm-csharp-orig-lsp-server ()
  (interactive)
  (setq lsp-csharp-server-path nil))

(defun sm-csharp-local-omnisharp-lsp-server ()
  (interactive)
  (setq lsp-csharp-server-path
        (expand-file-name
         "~/src/omnisharp-server-local/run")))

;;
;; key bindings
;;
(after! prodigy
  (set-evil-initial-state! 'prodigy-mode 'motion))

(after! docker
  (set-evil-initial-state! 'docker-mode 'motion))

(map!
 :nv [tab] #'indent-for-tab-command
 :nv ["C-i"] #'indent-for-tab-command ; tab is C-i in TTY mode
 :i "C-h" #'backward-delete-char
 :nm "C-]" #'+lookup/definition
 :nm "C-x ]" #'+default/search-project-for-symbol-at-point
 :nvmi "C-x C-d" #'dired-jump
 :ni "M-<up>" #'sm-move-line-up
 :ni "M-<down>" #'sm-move-line-down
 :nvmi "s-x" #'counsel-M-x
 :nvmi "s-e" #'+shell/toggle

 (:leader
   :desc "Switch to last buffer" :n  "TAB" #'mode-line-other-buffer

   "x" nil
   (:desc "text" :prefix "x"
     (:desc "align" :prefix "a"
       :desc "align-regexp" :nv "r" #'align-regexp))

   (:desc "flycheck/make" :prefix "e"
     :desc "List errors/diagnostics" :n "l" #'flycheck-list-errors
     :desc "Previous error" :n "p" #'flycheck-previous-error
     :desc "Next error" :n "n" #'flycheck-next-error)

   (:desc "git" :prefix "g"
     :desc "Git status" :n  "s" #'magit-status
     :desc "Git blame"  :n  "b" #'magit-blame)

   (:desc "open" :prefix "o"
     :desc "Prodigy services" :n "s" #'prodigy
     :desc "Mail" :n "m" #'mu4e)

   (:desc "toggle" :prefix "t"
     :desc "Line numbers" :n "l" #'sm-toggle-line-numbers)
   )

 (:after dired
   :map dired-mode-map
   :nm "o" #'dired-find-file
   :nm "O" #'dired-up-directory)

 (:after org
   :map evil-org-mode-map
   :i "C-h" #'backward-delete-char)

 (:after prodigy
   :map prodigy-mode-map
   :nm "$" #'prodigy-display-process)

 (:after prodigy
   :map prodigy-view-mode-map
   :nm "0" #'beginning-of-line
   :nm "C-a" #'beginning-of-line
   :nm "C-e" #'end-of-line)
)

(load! "+private.el")
