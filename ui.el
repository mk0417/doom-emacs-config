;;; ~/.doom.d/ui.el -*- lexical-binding: t; -*-

;; UI ----------------------------------------------------

;; theme
;; use `load-theme' instead of `setq doom-theme'
;; otherwise `set-face-attribute' does not work
;; https://github.com/hlissner/doom-emacs/issues/2194#issuecomment-565844321
;; (load-theme 'doom-one t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-wilmersdorf t)
;; (load-theme 'doom-tomorrow-night t)

;; (setq srcery-invert-region nil)
;; (setq srcery-invert-matches t)
;; (load-theme 'srcery t)

(setq modus-themes-syntax 'green-strings
      modus-themes-links 'faint
      ;; modus-themes-fringes 'intense
      modus-themes-prompts 'intense-accented
      modus-themes-intense-hl-line 't
      modus-themes-region 'bg-only)
(setq modus-themes-scale-headings t
      modus-themes-scale-1 1.3
      modus-themes-scale-2 1.1
      modus-themes-org-blocks 'grayscale)
(setq modus-themes-headings
      '((t . line)))
(load-theme 'modus-vivendi t)

;; diff-hl
;; diff-hl indicators disappear when using C-g to cancel popup windows
;; remove the hook below to fix the issue
(remove-hook '+popup-buffer-mode-hook #'+popup-adjust-margins-h)
;; disable fringe since diff-hl indicator is enough
;; (add-hook 'git-gutter-mode-hook (lambda () (fringe-mode '(0 . 0))))
;; https://www.reddit.com/r/emacs/comments/582yms/question_changing_the_colour_of_diffhl_indicators/d8x0fvd/
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(after! diff-hl
  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-insert ((t (:background "#568f56"))))
   '(diff-hl-delete ((t (:background "#ee6363"))))))

;; selected text color
(set-face-attribute 'region nil :background "#666666")

;; maximize window at startup
;; diable it if uisng yabai to manage window
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; prevents some cases of Emacs flickering
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; line number
(setq display-line-numbers-type t)

;; which-key delay
(setq which-key-idle-delay 0.5)

;; font
(defvar p-font)
;; (setq p-font "Cascadia Mono")
;; (setq p-font "mononoki")
;; (setq p-font "Iosevka Fixed SS12")
;; (setq p-font "Fira Code")
(setq p-font "DejaVu Sans Mono")
(setq doom-font (font-spec :family p-font :size 12)
      doom-variable-pitch-font (font-spec :family p-font :size 12))

;; line spaceing
(setq-default line-spacing 0)

;; column indicator
(setq-default display-fill-column-indicator-column 80)

;; change cursor color
;; https://github.com/hlissner/doom-emacs/issues/1848
(setq evil-normal-state-cursor '(box "#cf5a65")
      evil-insert-state-cursor '(bar "#cf5a65")
      evil-visual-state-cursor '(hollow "#cf5a65"))

;; highlight brackets
(after! paren
       (setq show-paren-delay 0
             show-paren-style 'parenthesis)
       (set-face-attribute 'show-paren-match nil :weight 'bold :background "#349cd9"))

;; company tooltip color
(after! company
  (custom-set-faces
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "#ffeead" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "#69adc6" :foreground "white"))))
   '(company-tooltip-annotation
     ((t (:background "#ffeead" :foreground "red"))))
   '(company-tooltip-common
     ((t (:background "#ffeead" :foreground "black"))))))

;; initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to EMACS\n"
              (format ";; Emacs version: %s\n" (car (split-string emacs-version)))
              (format ";; Packages: %d\n" (- (length load-path) (length doom--initial-load-path)))
              (format ";; System: %s\n" system-configuration)))

;; frame title
;; frame title is invisible when using Emacs 28 native-comp
;; build Emacs with no-titlebar
;; display frame title in my spacebar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat " " (abbreviate-file-name (buffer-file-name)))
                 " %b"))))

;; (setq frame-title-format nil)

;; minibuffer height
(after! ivy
  (setq ivy-height 9))

;; banner
;; (defun doom-dashboard-widget-banner ()
;;   (let ((point (point)))
;;     (mapc (lambda (line)
;;             (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
;;                                 'face 'doom-dashboard-banner) " ")
;;             (insert "\n"))
;;           '("Hello Peng, welcome to EMACS\n\n\n"))))

;; (defun doom-dashboard-widget-footer ()
;;   (insert "\n"))
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; modeline
;; doom-modeline
;; (after! doom-modeline
;;   (setq doom-modeline-modal-icon nil
;;         ;; doom-modeline-major-mode-icon t
;;         ;; doom-modeline--buffer-file-icon t
;;         ;; all-the-icons-scale-factor 0.5
;;         doom-modeline-buffer-modification-icon nil
;;         doom-modeline-height 6)
;;   (setq evil-normal-state-tag   (propertize "[Normal]")
;;         evil-insert-state-tag   (propertize "[Insert]")
;;         evil-visual-state-tag   (propertize "[Visual]")
;;         evil-motion-state-tag   (propertize "[Motion]")
;;         evil-operator-state-tag (propertize "[Operator]")
;;         evil-emacs-state-tag    (propertize "[Emacs]")))

;; (custom-set-faces!
;;   '(mode-line :height 1)
;;   '(mode-line-inactive :height 1))


;; custom modeline: version 1
;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; https://emacs.stackexchange.com/questions/38969/how-to-display-window-number-in-my-mode-line
;; (setq-default mode-line-format
;;   (list
;;    " "
;;    '(:eval (window-parameter (selected-window) 'ace-window-path))
;;    evil-mode-line-tag
;;    ;; the buffer name; the file name as a tool tip
;;    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
;;        'help-echo (buffer-file-name)))
;;    ;; line and column
;;    "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;    (propertize "%02l" 'face 'font-lock-type-face) ","
;;    (propertize "%02c" 'face 'font-lock-type-face)
;;    ") "
;;    ;; relative position, size of file
;;    "["
;;    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
;;    "/"
;;    (propertize "%I" 'face 'font-lock-constant-face) ;; size
;;    "] "
;;    ;; git branch
;;    '(vc-mode vc-mode)
;;    " "
;;    ;; the current major mode for the buffer.
;;    "["
;;    '(:eval (propertize "%m" 'face 'font-lock-string-face
;;        'help-echo buffer-file-coding-system))
;;    "] "
;;    ;; insert vs overwrite mode, input-method in a tooltip
;;    "["
;;    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;        'face 'font-lock-preprocessor-face
;;        'help-echo (concat "Buffer is in "
;;   (if overwrite-mode "overwrite" "insert") " mode")))

;;    ;; was this buffer modified since the last save?
;;    '(:eval (when (buffer-modified-p)
;;      (concat ","  (propertize "Mod"
;;       'face 'font-lock-warning-face
;;       'help-echo "Buffer has been modified"))))
;;    ;; is this buffer read-only?
;;    '(:eval (when buffer-read-only
;;              (concat ","  (propertize "RO"
;;       'face 'font-lock-type-face
;;       'help-echo "Buffer is read-only"))))
;;    "] "
;;     ;; add the time, with the date and the emacs uptime in the tooltip
;;     ;; '(:eval (propertize (format-time-string "%H:%M %a-%d-%m-%Y")
;;     ;;           'help-echo
;;     ;;           (concat (format-time-string "%c; ")
;;     ;;                   (emacs-uptime "Uptime:%hh"))))
;;     " --"
;;     ;; minor-mode-alist
;;     "%-"))


;; custom modeline: version 2
(setq-default mode-line-format
              (list
               " "
               '(:eval (window-parameter (selected-window) 'ace-window-path))
               ;; evil state indicator
               '(:eval (propertize evil-mode-line-tag))
               ;; the buffer name; the file name as a tool tip
               " "
               ;; project root
               (require 'projectile)
               '(:eval
                 (let ((face 'bold))
                   (when (projectile-project-name)
                     (concat
                      (propertize "[" 'face face)
                      (propertize (format "%s" (projectile-project-name)) 'face face)
                      (propertize "] " 'face face)))))
               " "
               '(:eval (propertize "%b  " 'help-echo (buffer-file-name)))
               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l") ","
               (propertize "%02c")
               ")  "
               ;; relative position, size of file
               ;; "["
               (propertize "%p") ;; % above top
               "/"
               (propertize "%I") ;; size
               ;; "] "
               "  "
               ;; git branch
               ;; '(vc-mode vc-mode)
               '(:eval (when-let (vc vc-mode)
                         ;; (list "Git:*" (propertize (substring vc 5) 'face "black") "*  ")))
                         (list "Git@" (propertize (substring vc 5) 'face "black") "  ")))

               ;; the current major mode for the buffer.
               ;; "["
               '(:eval (propertize "%m" 'help-echo buffer-file-coding-system))
               ;; "] "
               "  "
               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (propertize "(Mod)" 'help-echo "Buffer has been modified")))
               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (propertize "(RO)" 'help-echo "Buffer is read-only")))))

;; (set-face-attribute 'mode-line nil
;;                     ;; :underline "#898c8a"
;;                     ;; :overline "#898c8a"
;;                     :foreground "black"
;;                     :weight 'bold
;;                     :background "#898c8a")

;; Change modeline color based on evil state
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el
(defconst p-default-color (cons (face-background 'mode-line)
                                (face-foreground 'mode-line)))
(defun p-show-evil-state ()
  (let* ((color (cond ((minibufferp) p-default-color)
                      ;; ((evil-normal-state-p) '("grey80"  . "black"))
                      ((evil-insert-state-p) '("red"  . "#ffffff"))
                      ((evil-visual-state-p) '("#006fa0" . "#ffffff"))
                      ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                      ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                      (t p-default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))
    (set-face-bold 'mode-line t)
    (set-face-attribute 'mode-line nil :font "Menlo-12")))
(add-hook 'post-command-hook #'p-show-evil-state)


;; Transparency
;; @purcell
(defun p-adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
