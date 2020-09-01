;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+prog")
(load! "+ui")
(load! "+text")
(load! "+keybindings")


;; Company -------------------------------------------------
(after! company
 ;; (setq company-idle-delay 0.05
 ;;       company-minimum-prefix-length 1)
 (set-company-backend! '(prog-mode text-mode conf-mode)
   '(company-capf
     company-files
     company-keywords
     company-abbrev
     company-dabbrev
     company-dabbrev-code)))

;; fix bug when use company and fci-mode together
;; https://github.com/company-mode/company-mode/issues/180
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)


;; Evil ------------------------------------------------
;; (after! evil
;;   (setq-default evil-ex-search-persistent-highlight nil)
;;   (setq evil-ex-substitute-global t))


;; Visual-regexp ---------------------------------------
(after! visual-regexp
  (require 'visual-regexp))


;; Ivy -------------------------------------------------
(setq ivy-read-action-function #'ivy-hydra-read-action)


;; Projectile --------------------------------------------
(setq projectile-project-search-path '("~/Git/" "~/project/" "~/working/"))


;; iedit ---------------------------------------------
(require 'iedit)


;; evil-matchit ----------------------------------------
(require 'evil-matchit)
(after! evil-matchit
  (setq evilmi-shortcut "m")
  (global-evil-matchit-mode 1))


;; lsp ---------------------------------------------
;; https://github.com/emacs-lsp/lsp-mode/issues/1903
(setq lsp-enable-symbol-highlighting nil)


;; Misc ------------------------------------------------
(setq user-full-name "Peng"
      user-mail-address "pengdata1@gmail.com")

;; dashboard default directory
(setq +doom-dashboard-pwd-policy "~/")

;; activity-watch
(global-activity-watch-mode)

;; disable flyspell for git commit
(remove-hook! '(git-commit-mode-hook) #'flyspell-mode)

;; google this
(after! google-this
  (google-this-mode 1))

;; trash
(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;; maximum number of recent saved items
(setq recentf-max-saved-items 50)
(setq recentf-max-menu-items 50)

;; snippets
(setq yas-snippet-dirs '("~/.emacs.snippets"))

;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))


;; First input delay ------------------------------------
;; https://github.com/hlissner/doom-emacs/issues/3399
(run-hooks 'doom-first-input-hook)
