;;; ~/.doom.d/prog.el -*- lexical-binding: t; -*-

;; Functions
(defun p-jupyter-remove-line-overlay ()
  (interactive)
  (evil-open-below 0)
  (kill-whole-line)
  (evil-escape)
  (previous-line))
(defun p-jupyter-eval-block ()
  (interactive)
  (p-select-block)
  (let (beg end)
    (setq beg (region-beginning) end (region-end))
    (jupyter-eval-region beg end)))


;; jupyter
;; inline display
(setq jupyter-eval-use-overlays t)


;; Python --------------------------------------------------
;; enable elpy after python mode
;; startup time is reduced to 1.5s from 2.5s
(after! python
  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)
  ;; column indicator
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil)
  ;; @see https://github.com/nnicandro/emacs-jupyter/issues/270#issuecomment-697348350
  (set-popup-rule! "*jupyter-output*" :side 'right :size .35 :vslot 2)
  (map! :localleader
        (:map python-mode-map
         :prefix ("j" . "jupyter")
         :desc "run-jupyter"                     "j"         #'jupyter-run-repl
         :desc "eval-def"                        "f"         #'jupyter-eval-defun
         :desc "eval-line-or-region"             "r"         #'jupyter-eval-line-or-region
         :desc "eval-block"                      "e"         #'p-jupyter-eval-block
         :desc "restart-kernel"                  "R"         #'jupyter-repl-restart-kernel
         :desc "clear-cells"                     "K"         #'jupyter-repl-clear-cells
         :desc "interrupt-kernel"                "I"         #'jupyter-repl-interrupt-kernel
         :desc "inspect"                         "i"         #'jupyter-inspect-at-point
         :desc "remove-all-overlay"              "C"         #'jupyter-eval-remove-overlays
         :desc "remove-line-overlay"             "c"         #'p-jupyter-remove-line-overlay
         :desc "pop-to-repl"                     "w"         #'jupyter-repl-pop-to-buffer))

  ;; ;; elppy
  ;; (elpy-enable)
  ;; ;; Solve issue: send-region shows ^G
  ;; ;; https://github.com/jorgenschaefer/elpy/issues/1550#issuecomment-478448647
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"
  ;;       python-shell-prompt-detect-failure-warning nil
  ;;       ;; disable completion warning
  ;;       ;; https://github.com/jorgenschaefer/elpy/issues/887
  ;;       python-shell-completion-native-enable nil
  ;;       elpy-rpc-virtualenv-path 'current)
  ;; ;; disable flymake in Python
  ;; ;; https://github.com/jorgenschaefer/elpy/issues/828
  ;; (remove-hook 'elpy-modules 'elpy-module-flymake)
  ;; ;; disable highlight indentation
  ;; ;; https://stackoverflow.com/questions/45214116/how-to-disable-emacs-elpy-vertical-guide-lines-for-indentation
  ;; (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  ;; ;; Send current line
  ;; (defun p-elpy-shell-send-line ()
  ;;   (interactive)
  ;;   (progn
  ;;     (end-of-line)
  ;;     (set-mark (line-beginning-position)))
  ;;   (elpy-shell-send-region-or-buffer)
  ;;   (beginning-of-line)
  ;;   (keyboard-quit))
  ;; (define-key python-mode-map "\C-c\C-j" 'p-elpy-shell-send-line)
  ;; (general-create-definer p-python-leader-normal-def
  ;;   :prefix ","
  ;;   :states 'normal
  ;;   :keymaps 'python-mode-map)
  ;; (p-python-leader-normal-def
  ;;   "tp" 'run-python
  ;;   "rg" 'elpy-shell-send-group-and-step
  ;;   "rl" 'p-elpy-shell-send-line)
  ;; (general-create-definer p-python-leader-visual-def
  ;;   :prefix ","
  ;;   :states 'visual
  ;;   :keymaps 'python-mode-map)
  ;; (p-python-leader-visual-def
  ;;   "rr" 'elpy-shell-send-region-or-buffer)
  )


;; ESS ---------------------------------------------------
(after! ess
  ;; R
  (setq ess-ask-for-ess-directory nil)
  (add-hook 'ess-mode-hook 'display-fill-column-indicator-mode)
  ;; Stata
  ;; https://github.com/hieutkt/.doom.d/blob/master/config.el
  (setq inferior-STA-start-args ""
        inferior-STA-program (executable-find "stata")
        ;; fix: Error running timer 'ess--idle-timer-function': (wrong-type-argument stringp nil)
        ;; https://github.com/emacs-ess/ESS/issues/1102
        ess-can-eval-in-background nil)
  ;; keybindings
  (map! :localleader
        (:map ess-mode-map
         :desc "ess-cycle-assign"                "a"         #'ess-cycle-assign
         :prefix ("j" . "ess-eval")
         :desc "eval-def"                        "f"         #'ess-eval-function
         :desc "eval-line"                       "l"         #'ess-eval-line
         :desc "eval-line-or-region"             "r"         #'ess-eval-region-or-line-and-step)))


;; Go ---------------------------------------------------
(setq lsp-go-gopls-server-path "/Users/ml/go/bin/gopls")


;; Julia
(after! julia-mode
  ;; eglot-jl must know the environment in which it is installed
  ;; https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/julia
  (setq eglot-jl-language-server-project "~/.julia/environments/v1.5")
  ;; SymbolServer.jl takes a very long time to process project dependencies.
  ;; This is a one-time process that shouldn’t cause issues once the dependencies are cached,
  ;; however it can take over a minute to process each dependency
  ;; Increase to 60s
  ;; https://github.com/non-Jedi/eglot-jl
  (setq eglot-connect-timeout 60)
  (add-hook 'julia-mode-hook 'display-fill-column-indicator-mode)
  (map! :localleader
        (:map julia-mode-map
         :prefix ("j" . "jupyter")
         :desc "run-jupyter"                     "j"         #'jupyter-run-repl
         :desc "eval-def"                        "f"         #'jupyter-eval-defun
         :desc "eval-line-or-region"             "r"         #'jupyter-eval-line-or-region
         :desc "eval-block"                      "e"         #'p-jupyter-eval-block
         :desc "restart-kernel"                  "R"         #'jupyter-repl-restart-kernel
         :desc "clear-cells"                     "K"         #'jupyter-repl-clear-cells
         :desc "interrupt-kernel"                "I"         #'jupyter-repl-interrupt-kernel
         :desc "inspect"                         "i"         #'jupyter-inspect-at-point
         :desc "remove-all-overlay"              "C"         #'jupyter-eval-remove-overlays
         :desc "remove-line-overlay"             "c"         #'p-jupyter-remove-line-overlay
         :desc "pop-to-repl"                     "w"         #'jupyter-repl-pop-to-buffer)))
