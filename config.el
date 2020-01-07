;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Saulius Menkevičius"
      user-mail-address "sauliusmenkevicius@fastmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
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

;; system config
(when IS-MAC
  (add-to-list 'exec-path "/usr/local/share/dotnet")
  (add-to-list 'exec-path "/usr/local/bin")

  (setenv "PATH"
          (concat "/usr/local/share/dotnet" ":"
                  "/usr/local/bin" ":"
                  (getenv "PATH"))))

;; misc
(after! smartparens
  (smartparens-global-mode -1))

;; org mode
(setq org-directory "~/Dropbox/org/")

(after! org
  (setq org-startup-indented nil)
; (setq org-agenda-files '("~/Dropbox/org" "~/Dropbox/org/printlog"))
; (setq org-icalendar-combined-agenda-file "~/Dropbox/Public/org-calendar.ics")
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-agenda-default-appointment-duration 60))

;; mode line
(display-time)

;;
;; key bindings
;;

(defun sm-move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun sm-move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(map!
 :nv [tab] #'indent-for-tab-command
 :i "C-h" #'backward-delete-char
 :nm "C-]" #'+lookup/definition
 :nm "C-x ]" #'+default/search-project-for-symbol-at-point
 :nvmi "C-x C-d" #'dired-jump
 :ni "M-<up>" #'sm-move-line-up
 :ni "M-<down>" #'sm-move-line-down

 (:when IS-MAC
   :nvmi "s-x" 'counsel-M-x)

 (:leader
   ;;:desc "Switch to last buffer" :n  "TAB" #'wc/switch-to-mru-buffer

   (:desc "git" :prefix "g"
     :desc "Git status" :n  "s" #'magit-status
     :desc "Git blame"  :n  "b" #'magit-blame))

 (:after dired
   :map dired-mode-map
   :nm "o" #'dired-find-file
   :nm "O" #'dired-up-directory)
)
