;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+prog")
(load! "+ui")
(load! "+text")
(load! "+keybindings")


;; Company -------------------------------------------------
(after! company
  (set-company-backend! '(prog-mode text-mode conf-mode)
    '(company-files
      company-capf
      company-keywords
      company-dabbrev
      company-dabbrev-code
      company-abbrev)))


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


;; Misc ------------------------------------------------
(setq user-full-name "Peng"
      user-mail-address "pengdata1@gmail.com")

;; dashboard default directory
(setq +doom-dashboard-pwd-policy "~/")

;; activity-watch
(global-activity-watch-mode)

;; disable flyspell for git commit
(remove-hook! '(git-commit-mode-hook) #'flyspell-mode)

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
