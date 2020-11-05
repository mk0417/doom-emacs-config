;;; +ui.el -*- lexical-binding: t; -*-

;; UI ----------------------------------------------------

;; theme
;; use `load-theme' instead of `setq doom-theme'
;; otherwise `set-face-attribute' does not work
;; https://github.com/hlissner/doom-emacs/issues/2194#issuecomment-565844321
(load-theme 'doom-one t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'modus-operandi t)

;; selected text color
(set-face-attribute 'region nil :background "#666666")

;; maximize window at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; line number
(setq display-line-numbers-type t)

;; which-key delay
(setq which-key-idle-delay 0.5)

;; font
(defvar p-font)
;; (setq p-font "Cascadia Mono")
;; (setq p-font "mononoki")
;; (setq p-font "Iosevka Fixed SS12")
(setq p-font "Fira Code")
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
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight normal))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))

;; initial scratch buffer message
(setq initial-scratch-message ";;; Hello Peng, welcome to EMACS\n")

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

;; frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat " " (abbreviate-file-name (buffer-file-name)))
                 " %b"))))

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
                         (list "Git:*" (propertize (substring vc 5)) "*")))
               "  "
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
                      ((evil-normal-state-p) '("grey70"  . "black"))
                      ((evil-insert-state-p) '("coral1"  . "#ffffff"))
                      ((evil-visual-state-p) '("#006fa0" . "#ffffff"))
                      ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                      ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                      (t p-default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))
    (set-face-bold 'mode-line t)
    (set-face-attribute 'mode-line nil :font "Fira Code-11")))
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
