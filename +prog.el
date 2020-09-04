;;; +python.el -*- lexical-binding: t; -*-

;; Python --------------------------------------------------

;; Send current line
(defun p-elpy-shell-send-line ()
  (interactive)
  (progn
    (end-of-line)
    (set-mark (line-beginning-position)))
  (elpy-shell-send-region-or-buffer)
  (beginning-of-line)
  (keyboard-quit))

;; enable elpy after python mode
;; startup time is reduced to 1.5s from 2.5s
(after! python
  (elpy-enable)
  ;; Solve issue: send-region shows ^G
  ;; https://github.com/jorgenschaefer/elpy/issues/1550#issuecomment-478448647
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"
        python-shell-prompt-detect-failure-warning nil)
  ;; disable flymake in Python
  ;; https://github.com/jorgenschaefer/elpy/issues/828
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  ;; disable highlight indentation
  ;; https://stackoverflow.com/questions/45214116/how-to-disable-emacs-elpy-vertical-guide-lines-for-indentation
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  (setq elpy-rpc-virtualenv-path 'current)
  (define-key python-mode-map "\C-c\C-j" 'p-elpy-shell-send-line))

(general-create-definer p-python-leader-normal-def
  :prefix ","
  :states 'normal
  :keymaps 'python-mode-map)
(p-python-leader-normal-def
  "tp" 'run-python
  "rg" 'elpy-shell-send-group-and-step
  "rl" 'p-elpy-shell-send-line)

(general-create-definer p-python-leader-visual-def
  :prefix ","
  :states 'visual
  :keymaps 'python-mode-map)
(p-python-leader-visual-def
  "rr" 'elpy-shell-send-region-or-buffer)


;; ESS ---------------------------------------------------

(after! ess
  (setq ess-ask-for-ess-directory nil))

(general-create-definer p-ess-leader-def
  :prefix ","
  :states '(normal visual)
  :keymaps 'ess-mode-map)
(p-ess-leader-def
  "rl" 'ess-eval-line
  "rr" 'ess-eval-region
  "a" 'ess-cycle-assign)
