;;; +ui.el -*- lexical-binding: t; -*-

;; UI ----------------------------------------------------

;; theme
(setq doom-theme 'doom-one)

;; (setq srcery-invert-region nil)
;; (setq doom-theme 'srcery)

;; maximize window at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; line number
(setq display-line-numbers-type t)

;; which-key delay
(setq which-key-idle-delay 0.5)

;; font
(setq doom-font (font-spec :family "Cascadia Mono" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Cascadia Mono" :size 12))

;; fci rule
(setq fci-rule-column 80)

;; change cursor color
;; https://github.com/hlissner/doom-emacs/issues/1848
(setq evil-normal-state-cursor '(box "#cf5a65")
      evil-insert-state-cursor '(bar "#cf5a65")
      evil-visual-state-cursor '(hollow "#cf5a65"))

;; banner
(defun doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '("Hello Peng, welcome to EMACS\n\n\n"))
    ))
(defun doom-dashboard-widget-footer ()
  (insert "\n"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; modeline
(after! doom-modeline
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-modal-icon nil))

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
;;     '(:eval (propertize (format-time-string "%H:%M  %a-%d-%m-%Y")
;;               'help-echo
;;               (concat (format-time-string "%c; ")
;;                       (emacs-uptime "Uptime:%hh"))))
;;     " --"
;;     ;; minor-mode-alist
;;     "%-"))
