;;; +keybindings.el -*- lexical-binding: t; -*-

;; Keybindings --------------------------------------------

;; MacOS meta
(setq ns-option-modifier nil
      ns-right-option-modifier 'meta)

;; evil-escape
(setq-default evil-escape-key-sequence "fd")

;; non-leader keybindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "s-C-i") (lambda () (interactive) (p-adjust-opacity nil -2)))
(global-set-key (kbd "s-C-o") (lambda () (interactive) (p-adjust-opacity nil 2)))
(global-set-key (kbd "s-C-u") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(after! evil
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "gn") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "gp") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "gl") 'evil-shift-right)
  (define-key evil-normal-state-map (kbd "gh") 'evil-shift-left)
  (define-key evil-normal-state-map (kbd "C-c j") (lambda() (interactive) (scroll-other-window 2)))
  (define-key evil-normal-state-map (kbd "C-c J") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "C-c k") (lambda() (interactive) (scroll-other-window-down 2)))
  (define-key evil-normal-state-map (kbd "C-c K") 'scroll-other-window-down)
  (define-key evil-normal-state-map (kbd "gon") 'evil-multiedit-match-symbol-and-next)
  (define-key evil-normal-state-map (kbd "gop") 'evil-multiedit-match-symbol-and-prev)
  (define-key evil-normal-state-map (kbd "god") 'p-delete-parens)
  (define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
  (define-key evil-normal-state-map (kbd "goi") 'p-select-text-in-quote)
  (define-key evil-normal-state-map (kbd "gos") 'p-select-block)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "gl") 'evil-shift-right)
  (define-key evil-visual-state-map (kbd "gh") 'evil-shift-left)
  (define-key evil-visual-state-map (kbd "fd") 'evil-escape)
  (define-key evil-visual-state-map (kbd "gok") 'p-surround-parens)
  (define-key evil-visual-state-map (kbd "gof") 'p-surround-brackets)
  (define-key evil-visual-state-map (kbd "goh") 'p-surround-curly)
  (define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
  (define-key evil-insert-state-map (kbd "C-l") 'p-delete-backward-to-tab))

(after! ivy
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done))

(after! company
  (define-key company-active-map (kbd "C-j") #'company-complete-selection))

(after! dired
  (define-key dired-mode-map (kbd "C-c <return>") 'p-open-in-external-app))

;; leader key
(map! :leader
      :desc "winum-select-window-1"                "1"      #'winum-select-window-1
      :desc "winum-select-window-2"                "2"      #'winum-select-window-2
      :desc "winum-select-window-3"                "3"      #'winum-select-window-3
      :desc "winum-select-window-4"                "4"      #'winum-select-window-4
      :desc "M-x"                                  "SPC"    #'execute-extended-command
      :desc "Find file in project"                 ":"      #'projectile-find-file
      (:prefix-map ("b" . "buffer")
       (:when (featurep! :ui workspaces)
        :desc "Switch workspace buffer"            "B"      #'persp-switch-to-buffer
       :desc "Switch buffer"                       "b"      #'switch-to-buffer)
       :desc "narrow region"                       "n"      #'+evil:narrow-buffer
       :desc "widen"                               "w"      #'widen
       (:unless (featurep! :ui workspaces)
        :desc "Switch buffer"                      "b"      #'switch-to-buffer)
       :desc "eval buffer"                         "e"      #'eval-buffer
       :desc "switch to dashboard"                 "a"      #'+doom-dashboard/open
       :desc "switch to scratch"                   "s"      #'p-switch-to-scratch
       :desc "kill buffer and window"              "D"      #'kill-buffer-and-window)
      (:prefix-map ("e" . "text")
       :desc "beginning-of-defun"                  "b"      #'beginning-of-defun
       :desc "end-of-defun"                        "e"      #'end-of-defun
       :desc "p-select-functoin"                   "f"      #'p-select-function
       :desc "evilmi-select-items"                 "s"      #'evilmi-select-items)
      (:prefix-map ("s" . "search")
       :desc "swiper-isearch-thing-at-point"       "S"      #'swiper-isearch-thing-at-point
       :desc "search-project-at-point"             "a"      #'+default/search-project-for-symbol-at-point
       :desc "p-google-search"                     "g"      #'p-google-search
       :desc "p-youtube-search"                    "y"      #'p-youtube-search
       :desc "counsel-yank-pop"                    "h"      #'counsel-yank-pop)
      (:prefix-map ("n" . "notes")
       :desc "olivetti"                            "O"      #'olivetti-mode
       :desc "org-tree-slide-mode"                 "P"      #'org-tree-slide-mode
       :desc "open journal file"                   "jo"     #'org-journal-open-current-journal-file
       (:prefix ("p" . "pandoc")
        :desc "pandoc-set-format"                  "f"      #'pandoc-set-write
        :desc "pandoc-set-output-name"             "n"      #'pandoc-set-output
        :desc "pandoc-set-output-directory"        "d"      #'pandoc-set-output-dir
        :desc "pandoc-run"                         "r"      #'pandoc-run-pandoc))
      (:prefix-map ("i" . "insert")
       :desc "insert uk date"                      "k"      #'p-insert-uk-date
       :desc "insert date"                         "d"      #'p-insert-date)
      (:prefix-map ("f" . "file")
       :desc "locate file"                         "g"      #'locate-file
       :desc "p-counsel-find-literature"           "l"      #'p-counsel-find-literature
       :desc "p-dired-jump-literature"             "L"      #'p-dired-jump-literature
       :desc "dired jump"                          "j"      #'dired-jump)
      (:prefix-map ("t" . "toggle")
       :desc "World time"                          "W"      #'display-time-world
       :desc "maximize frame"                      "m"      #'toggle-frame-maximized)
      (:prefix-map ("g" . "git")
       :desc "Magit diff"                         "d"      #'magit-diff))

;; local-leader
(setq doom-localleader-key ";")

;; dired mode
(map! :localleader
      (:map dired-mode-map
       :desc "dired-downcase"                             "l"      #'dired-downcase
       :desc "dired-upcase"                               "u"      #'dired-upcase
       :desc "dired-create-empty-file"                    "n"      #'dired-create-empty-file
       :desc "dired-create-directory"                     "a"      #'dired-create-directory
       :desc "dired-mark-files-regexp"                    "f"      #'dired-mark-files-regexp
       :desc "dired-mark-files-containing-regexp"         "c"      #'dired-mark-files-containing-regexp
       :desc "dired-up-directory"                         "p"      #'dired-up-directory))

;; latex mode
(map! :map LaTeX-mode-map
      :localleader
      :desc "latex-preview-pane-mode"                    "j"      #'latex-preview-pane-mode)


;; Efficient typing -------------------------------------
(after! general
  (general-evil-setup t)
  ;; parenthesis
  (defun p-insert-paren ()
    (interactive)
    (insert "()")
    (backward-char 1))
  (general-imap "k"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'p-insert-paren))
  ;; square brackets
  (defun p-insert-sbracket ()
    (interactive)
    (insert "[]")
    (backward-char 1))
  (general-imap "i"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "i" 'p-insert-sbracket))
  ;; curly brackets
  (defun p-insert-cbracket ()
    (interactive)
    (insert "{}")
    (backward-char 1))
  (general-imap "h"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "h" 'p-insert-cbracket))
  ;; exclamation
  (defun p-insert-exc ()
    (interactive)
    (insert "!"))
  (general-imap "g"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "t" 'p-insert-exc))
  ;; at
  (defun p-insert-at ()
    (interactive)
    (insert "@"))
  (general-imap "q"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "a" 'p-insert-at))
  ;; british pound
  (defun p-insert-pound ()
    (interactive)
    (insert "£"))
  (general-imap "y"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "b" 'p-insert-pound))
  ;; dollar
  (defun p-insert-dollar ()
    (interactive)
    (insert "$"))
  (general-imap "l"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'p-insert-dollar))
  ;; percentage
  (defun p-insert-percent ()
    (interactive)
    (insert "%"))
  (general-imap "f"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "h" 'p-insert-percent))
  ;; carat
  (defun p-insert-carat ()
    (interactive)
    (insert "^"))
  (general-imap "p"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-carat))
  ;; and
  (defun p-insert-and ()
    (interactive)
    (insert "&"))
  (general-imap "a"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "a" 'p-insert-and))
  ;; asterisk
  (defun p-insert-asterisk ()
    (interactive)
    (insert "*"))
  (general-imap "c"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-asterisk))
  ;; underscore
  (defun p-insert-underscore ()
    (interactive)
    (insert "_"))
  (general-imap "u"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "u" 'p-insert-underscore))
  ;; plus
  (defun p-insert-plus ()
    (interactive)
    (insert "+"))
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-plus))
  ;; pipe
  (defun p-insert-pipe ()
    (interactive)
    (insert "|"))
  (general-imap "s"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "g" 'p-insert-pipe))
  ;; tilde
  (defun p-insert-tilde ()
    (interactive)
    (insert "~"))
  (general-imap "b"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-tilde))
  ;; less than
  (defun p-insert-less ()
    (interactive)
    (insert "<"))
  (general-imap "x"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "y" 'p-insert-less))
  ;; greater than
  (defun p-insert-greater ()
    (interactive)
    (insert ">"))
  (general-imap "d"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "y" 'p-insert-greater))
  ;; question
  (defun p-insert-question ()
    (interactive)
    (insert "?"))
  (general-imap "w"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-question))
  ;; r assign
  (defun p-insert-r-assign ()
    (interactive)
    (insert "<-"))
  (general-imap "e"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-r-assign))
  ;; r connect
  (defun p-insert-r-connect ()
    (interactive)
    (insert "%>%"))
  (general-imap "r"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-r-connect))
  ;; hash
  (defun p-insert-hash ()
    (interactive)
    (insert "#"))
  (general-imap "v"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "v" 'p-insert-hash)))
