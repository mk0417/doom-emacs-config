;;; +python.el -*- lexical-binding: t; -*-

;; Python --------------------------------------------------

;; enable elpy after python mode
;; startup time is reduced to 1.5s from 2.5s
(after! python
  ;; elppy
  (elpy-enable)
  ;; Solve issue: send-region shows ^G
  ;; https://github.com/jorgenschaefer/elpy/issues/1550#issuecomment-478448647
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"
        python-shell-prompt-detect-failure-warning nil
        ;; disable completion warning
        ;; https://github.com/jorgenschaefer/elpy/issues/887
        python-shell-completion-native-enable nil
        elpy-rpc-virtualenv-path 'current)
  ;; disable flymake in Python
  ;; https://github.com/jorgenschaefer/elpy/issues/828
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  ;; disable highlight indentation
  ;; https://stackoverflow.com/questions/45214116/how-to-disable-emacs-elpy-vertical-guide-lines-for-indentation
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  ;; Send current line
  (defun p-elpy-shell-send-line ()
    (interactive)
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))
    (elpy-shell-send-region-or-buffer)
    (beginning-of-line)
    (keyboard-quit))
  (define-key python-mode-map "\C-c\C-j" 'p-elpy-shell-send-line)
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
  ;; jupyter
  (setq jupyter-eval-use-overlays t)
  (map! :localleader
        (:map python-mode-map
         :prefix ("j" . "jupyter")
         :desc "run-jupyter"                     "j"         #'jupyter-run-repl
         :desc "eval-def"                        "f"         #'jupyter-eval-defun
         :desc "eval-line-or-region"             "r"         #'jupyter-eval-line-or-region
         :desc "restart-kernel"                  "R"         #'jupyter-repl-restart-kernel
         :desc "clear-cells"                     "C"         #'jupyter-repl-clear-cells
         :desc "interrupt-kernel"                "I"         #'jupyter-interrupt-kernel
         :desc "inspect"                         "i"         #'jupyter-inspect-at-point
         :desc "remove-overlay"                  "c"         #'jupyter-eval-remove-overlays)))


;; ESS ---------------------------------------------------
(after! ess
  (setq ess-ask-for-ess-directory nil)
  (general-create-definer p-ess-leader-def
    :prefix ","
    :states '(normal visual)
    :keymaps 'ess-mode-map)
  (p-ess-leader-def
    "rl" 'ess-eval-line
    "rr" 'ess-eval-region
    "a" 'ess-cycle-assign))
