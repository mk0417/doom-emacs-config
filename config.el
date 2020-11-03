;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+prog")
(load! "+ui")
(load! "+text")
(load! "+keybindings")


;; Company -------------------------------------------------
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  (set-company-backend! '(prog-mode text-mode conf-mode)
    '(company-files
      company-capf
      company-keywords
      company-dabbrev
      company-dabbrev-code
      company-abbrev)))


;; Eglot --------------------------------------------------
;; https://github.com/joaotavora/eglot/issues/334
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
;; https://github.com/joaotavora/eglot/issues/324
(setq eglot-stay-out-of '(company))
;; https://github.com/joaotavora/eglot/pull/459
(setq eldoc-echo-area-use-multiline-p nil)


;; Ivy -------------------------------------------------
(setq ivy-read-action-function #'ivy-hydra-read-action)


;; Projectile --------------------------------------------
(setq projectile-project-search-path '("~/Git/" "~/project/" "~/working/"))


;; evil-matchit ----------------------------------------
(setq evilmi-shortcut "m")
(add-hook 'prog-mode-hook 'evil-matchit-mode)


;; Misc ------------------------------------------------
(setq user-full-name "Peng"
      user-mail-address "pengdata1@gmail.com")

;; from https://blog.sumtypeofway.com/posts/emacs-config.html
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)
(setq require-final-newline t)

;; use home as default directory
(setq default-directory "~/")

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
(after! recentf
  (setq recentf-max-saved-items 50))

;; snippets
(setq yas-snippet-dirs '("~/.emacs.snippets"))

;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; open from external app (Xah Lee)
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun p-open-in-external-app (&optional @fname)
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))))))


;; First input delay ------------------------------------
;; https://github.com/hlissner/doom-emacs/issues/3399
(run-hooks 'doom-first-input-hook)
