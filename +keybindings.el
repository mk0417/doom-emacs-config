;;; +keybindings.el -*- lexical-binding: t; -*-

;; Keybindings --------------------------------------------

;; MacOS meta
(setq ns-option-modifier nil
      ns-right-option-modifier 'meta)

;; non-leader keybindings
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "C-j") 'transpose-words)
(define-key evil-normal-state-map (kbd "C-;") 'iedit-mode-toggle-on-function)
(define-key evil-normal-state-map (kbd "s-;") 'iedit-mode)
(define-key evil-normal-state-map (kbd "gn") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "gp") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd ";l") 'evil-shift-right)
(define-key evil-normal-state-map (kbd ";h") 'evil-shift-left)
(define-key evil-normal-state-map (kbd "C-0") 'doom/reset-font-size)
(define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "v")   'er/expand-region)
(define-key evil-visual-state-map (kbd ";l") 'evil-shift-right)
(define-key evil-visual-state-map (kbd ";h") 'evil-shift-left)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'org-capture)

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
        :desc "Switch buffer"                      "b"      #'switch-to-buffer)
       :desc "narrow region"                       "n"      #'+evil:narrow-buffer
       :desc "widen"                               "w"      #'widen
       (:unless (featurep! :ui workspaces)
        :desc "Switch buffer"                      "b"      #'switch-to-buffer)
       :desc "eval buffer"                         "e"      #'eval-buffer
       :desc "switch to dashboard"                 "s"      #'+doom-dashboard/open)
      (:prefix-map ("e" . "text")
       :desc "beginning-of-defun"                  "b"      #'beginning-of-defun
       :desc "end-of-defun"                        "e"      #'end-of-defun
       :desc "vr/replace"                          "r"      #'vr/replace
       :desc "evilmi-select-items"                 "s"      #'evilmi-select-items)
      (:prefix-map ("s" . "search")
       :desc "swiper-isearch-thing-at-point"       "S"      #'swiper-isearch-thing-at-point
       :desc "search-project-at-point"             "a"      #'+default/search-project-for-symbol-at-point
       :desc "counsel-file-jump"                   "g"      #'counsel-file-jump
       :desc "counsel-yank-pop"                    "h"      #'counsel-yank-pop)
      (:prefix-map ("n" . "notes")
       :desc "new deft file"                       "D"      #'deft-new-file
       :desc "olivetti"                            "O"      #'olivetti-mode
       :desc "org-tree-slide-mode"                 "p"      #'org-tree-slide-mode)
      (:prefix-map ("i" . "insert")
       :desc "insert uk date"                      "k"      #'p-insert-uk-date
       :desc "insert date"                         "d"      #'p-insert-date))

;; local-leader
(setq doom-localleader-key ",")


;; Efficient typing -------------------------------------
(require 'general)
(general-evil-setup t)

;; parenthesis
(defun p-insert-paren ()
  (interactive)
  (insert "()"))
(general-imap "k"
  (general-key-dispatch 'self-insert-command
  :timeout 0.25
  "h" 'p-insert-paren))
;; curly brackets
(defun p-insert-cbracket ()
  (interactive)
  (insert "{}"))
(general-imap "h"
  (general-key-dispatch 'self-insert-command
  :timeout 0.25
  "k" 'p-insert-cbracket))
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
; dollar
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
  "h" 'p-insert-and))
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
  "d" 'p-insert-underscore))
;; plus
(defun p-insert-plus ()
  (interactive)
  (insert "+"))
(general-imap "j"
  (general-key-dispatch 'self-insert-command
  :timeout 0.25
  "i" 'p-insert-plus))
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
  "n" 'p-insert-question))
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
  "v" 'p-insert-hash))
