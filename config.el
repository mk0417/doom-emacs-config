;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+prog")
(load! "+ui")
(load! "+text")
(load! "+keybindings")


;; Company -------------------------------------------------
(after! company
 (setq company-idle-delay 0.05
       company-minimum-prefix-length 1)
 (set-company-backend! '(prog-mode text-mode conf-mode)
   '(company-capf
     company-files
     company-keywords
     company-abbrev
     company-dabbrev
     company-dabbrev-code)))


;; Evil ------------------------------------------------
(after! evil
  (setq-default evil-ex-search-persistent-highlight nil)
  (setq evil-ex-substitute-global t))


;; Visual-regexp ------------------------------------------------
(after! visual-regexp
  (require 'visual-regexp))


;; Ivy -------------------------------------------------
(setq ivy-read-action-function #'ivy-hydra-read-action)


;; Projectile --------------------------------------------
(setq projectile-project-search-path '("~/Git/" "~/project/" "~/working/"))


;; iedit ---------------------------------------------
(require 'iedit)


;; Misc ------------------------------------------------
(setq +doom-dashboard-pwd-policy "~/")

(global-activity-watch-mode)

(remove-hook! '(git-commit-mode-hook) #'flyspell-mode)

(google-this-mode 1)

;; trash
(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;; maximum number of recent saved items
(setq recentf-max-saved-items 50)


;; First input delay ------------------------------------
;; https://github.com/hlissner/doom-emacs/issues/3399
(run-hooks 'doom-first-input-hook)
