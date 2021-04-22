;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load "~/.doom.d/lisp/p-ui.el" t t)
(load "~/.doom.d/lisp/p-keybindings.el" t t)
(load "~/.doom.d/lisp/p-prog.el" t t)
(load "~/.doom.d/lisp/p-text.el" t t)
(load "~/.doom.d/lisp/p-abbrev.el" t t)


;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (>= emacs-major-version 28)
    (setq comp-deferred-compilation t
          package-native-compile t
          load-prefer-newer t))

;; don't GC during startup to save time
;; https://github.com/redguardtoo/emacs.d/blob/master/init.el
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


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
;; disable highlight at point
;; https://github.com/joaotavora/eglot/issues/334
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
;; Do not overwrite company-backends
;; https://github.com/joaotavora/eglot/issues/324
(setq eglot-stay-out-of '(company))
;; https://github.com/joaotavora/eglot/pull/459
(setq eldoc-echo-area-use-multiline-p nil)
;; disable flymake
(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))


;; Ivy -------------------------------------------------
(setq ivy-read-action-function #'ivy-hydra-read-action)


;; Projectile --------------------------------------------
(setq projectile-project-search-path '("~/Git/"))


;; evil-matchit ----------------------------------------
(setq evilmi-shortcut "m")
(add-hook 'prog-mode-hook 'evil-matchit-mode)


;; grugru ----------------------------------------
(grugru-define-global 'symbol '("True" "False"))
(grugru-define-global 'symbol '("true" "false"))
(grugru-define-global 'symbol '("mean" "median"))
(grugru-define-global 'symbol '("min" "max"))
(grugru-define-global 'symbol '("<=" ">="))
(grugru-define-global 'symbol '("<" ">"))
(grugru-define-global 'symbol '("any" "all"))
(grugru-define-global 'symbol '("int" "str"))
(grugru-define-global 'symbol '("params" "tvalues" "pvalues"))
(grugru-define-global 'word '("ew" "vw"))
(grugru-define-global 'word '("EW" "VW"))


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
;; (setq +doom-dashboard-pwd-policy "~/")

;; activity-watch
;; disable global-activity-watch-mode to improve startup time
;; (add-hook 'prog-mode-hook 'activity-watch-mode)
;; (add-hook 'org-mode-hook 'activity-watch-mode)

;; disable flyspell for git commit
(remove-hook! '(git-commit-mode-hook) #'flyspell-mode)

;; trash
(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;; maximum number of recent saved items
(after! recentf
  (setq recentf-max-saved-items 50)
  (add-to-list 'recentf-exclude "emacs-everywhere"))

;; snippets
(setq yas-snippet-dirs '("~/.emacs.snippets"))

;; world time
(setq display-time-world-list
  '(("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("Europe/London" "London")
    ("Europe/Paris" "Paris")
    ("Asia/Shanghai" "Shanghai")
    ("Asia/Tokyo" "Tokyo")
    ("Pacific/Auckland" "Auckland")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; yankpad
(after! yankpad
  (setq yankpad-file "~/org/yankpad.org"))

;; connect to wrds database
(setq sql-connection-alist
      '((wrds-pg (sql-product 'postgres)
                    (sql-username (with-temp-buffer (insert-file-contents "~/.pass/wrds/user.txt") (buffer-string)))
                    (sql-password (with-temp-buffer (insert-file-contents "~/.pass/wrds/password.txt") (buffer-string)))
                    (sql-server "wrds-pgdata.wharton.upenn.edu")
                    (sql-port 9737)
                    (sql-database "wrds")
                    (sql-sslmode "require"))))

;; abbrev mode
(setq-default abbrev-mode t)

;; color-rg
;; also need exec-path-from-shell
;; reduce exec-path-from-shell startup time
(setq exec-path-from-shell-arguments nil)
(require 'color-rg)
;; https://emacs.stackexchange.com/questions/31244/how-can-i-disable-evil-in-help-mode
(evil-set-initial-state 'color-rg-mode 'emacs)

;; better scrolling performance
;; https://github.com/mpereira/.emacs.d/#a-fast-non-projectile-based-project-file-finder
;; https://www.reddit.com/r/emacs/comments/gaub11/poor_scrolling_performance_in_doom_emacs/fp392eh/
;; (setq jit-lock-defer-time 0)

;; https://github.com/Townk/doom-emacs-private/blob/master/config.org
(setq ns-use-thin-smoothing t)

;; Functions
;; https://stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs
(defun p-surround-parens ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\( ?\))
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\( ?\)))))

(defun p-surround-brackets ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\[ ?\])
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\[ ?\]))))

(defun p-surround-curly ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\{ ?\})
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\{ ?\}))))

(defun p-surround-asterisk ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\* ?\*)
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\* ?\*))))

(defun p-surround-slash ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\/ ?\/)
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\/ ?\/))))

(defun p-surround-equal ()
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 ?\= ?\=)
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 ?\= ?\=))))

;; https://emacs.stackexchange.com/questions/54659/how-to-delete-surrounding-brackets
(defun p-delete-parens ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (let ((beg (point)))
      (forward-list)
      (delete-backward-char 1)
      (goto-char beg)
      (delete-char 1))))

;; ex-evil replace
(defun p-ex-evil-buffer-replace ()
  (interactive)
  (evil-ex (concat "%s/")))

(defun p-ex-evil-selection-replace ()
  (interactive)
  (evil-ex (concat "'<,'>s/")))

;; switch to scratch buffer
(defun p-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; counsel find my literature
(defun p-counsel-find-literature ()
  (interactive)
  (counsel-find-file "~/Dropbox/roam_literature"))

;; dired open my literature
(defun p-dired-jump-literature ()
  (interactive)
  (dired "~/Dropbox/roam_literature"))

;; delete to tab
(defun p-delete-backward-to-tab ()
  (interactive)
  (kill-line 0)
  (insert "    "))

;; select functions
(defun p-select-function ()
  (interactive)
  (beginning-of-defun)
  (evilmi-select-items))

;; select text in qutoe: Xah Lee
;; http://ergoemacs.org/emacs/modernization_mark-word.html
(defun p-select-text-in-quote ()
  (interactive)
  (let (($skipChars "^'\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
        〘〙")
        $p1)
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)
    (backward-char)))

;; select block between blank lines: Xah Lee
;; http://ergoemacs.org/emacs/modernization_mark-word.html
(defun p-select-block ()
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move")
      (forward-line -1))))

;; keep unique lines
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-misc.el
(defun p-uniq-lines (start end)
  (interactive "*r")
  (delete-duplicate-lines start end))

;; add four spaces
(defun p-insert-spaces ()
  (interactive)
  (insert "    "))

;; google search
;; https://emacsredux.com/blog/2013/03/28/google/
(defun p-google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; youtube search
;; https://emacsredux.com/blog/2013/08/26/search-youtube/
(defun p-youtube-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

;; open using external app in dired
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun p-open-in-external-app (&optional @fname)
  (interactive)
  (let* (($file-list
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
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))
         $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath)))
         $file-list))))))


;; First input delay ------------------------------------
;; https://github.com/hlissner/doom-emacs/issues/3399
;; (run-hooks 'doom-first-input-hook)
