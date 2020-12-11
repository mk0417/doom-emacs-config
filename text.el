;;; +org.el -*- lexical-binding: t; -*-

;; Org ---------------------------------------------------

;; To modify org-directory it must be set before org has loaded
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/org
(setq org-directory "~/org/agenda"
      org-agenda-files '("~/org/agenda/meeting.org"
                         "~/org/agenda/todo.org"
                         "~/org/agenda/routine.org")
      org-roam-directory "~/org/roam/"
      org-roam-db-location "~/org/roam/org-roam.db"
      org-journal-dir "~/org/journal"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-file-type 'monthly
      org-journal-enable-agenda-integration t)

(after! org
  (setq org-superstar-remove-leading-stars t
        org-superstar-headline-bullets-list '("◉" "○" "▷")
        org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–)))
  (defun p-org-presentation-on ()
    (interactive)
    (progn
      (org-tree-slide-mode 1)
      (olivetti-mode 1)))
  (defun p-org-presentation-off ()
    (interactive)
    (progn
      (org-tree-slide-mode -1)
      (olivetti-mode -1)))
  (setq org-tree-slide-breadcrumbs nil
        org-tree-slide-header nil
        org-tree-slide-slide-in-effect nil
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init t
        org-tree-slide-modeline-display nil
        org-tree-slide-skip-done nil
        org-tree-slide-skip-comments t
        org-tree-slide-fold-subtrees-skipped t
        org-tree-slide-skip-outline-level 8
        org-tree-slide-never-touch-face t
        org-tree-slide-activate-message
        (propertize "Presentation mode ON" 'face 'success)
        org-tree-slide-deactivate-message
        (propertize "Presentation mode OFF" 'face 'error))
  ;; (setq org-roam-server-host "127.0.0.1"
  ;;       org-roam-server-port 8080
  ;;       org-roam-server-authenticate nil
  ;;       org-roam-server-export-inline-images t
  ;;       org-roam-server-serve-files nil
  ;;       org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
  ;;       org-roam-server-network-poll t
  ;;       org-roam-server-network-arrows nil
  ;;       org-roam-server-network-label-truncate t
  ;;       org-roam-server-network-label-truncate-length 60
  ;;       org-roam-server-network-label-wrap-length 20)
  ;; overwrite default templates
  ;; does not work if move it out of after! org
  (setq org-capture-templates
        '(("m" "Meeting and event" entry
           (file+headline "meeting.org" "Meetings, events, and appointments")
           "* %^{Scope of meeting|Event: |Research discussion: |Staff meeting: |Student meeting: } %^{Title} %^g\nSCHEDULED: %^t\n")
          ("t" "TODO" entry
           (file+headline "todo.org" "Todo and task")
           "* TODO %^{Title} \nSCHEDULED: %^t\n")
          ("r" "Routine" entry
           (file+headline "routine.org" "Routine")
           "* TODO %^{Title} \nSCHEDULED: %^t\n :PROPERTIES:\n :STYLE:    habit\n :END:\n")))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  ;; disable line number in org mode
  ;; https://github.com/hlissner/doom-emacs/issues/827#issuecomment-653784466
  (add-hook! 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook 'text-mode-hook 'pandoc-mode)
  ;; org-reveal
  (setq org-reveal-mathjax t)
  (setq plantuml-jar-path (expand-file-name "~/plantuml.jar"))
  ;; org-habit
  (add-to-list 'org-modules 'org-habit)
  ;; sorce block template
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("b" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("j" . "src jupyter-python :session py"))
  ;; keybindings
  ;; cause warning of `failed to load org package incrementally' if move out of after! org
  (map! :localleader
        (:map org-mode-map
         ;; presentation mode
         :desc "next slide"                             "l"         #'org-tree-slide-move-next-tree
         :desc "next slide"                             "h"         #'org-tree-slide-move-previous-tree
         :desc "presentation mode on"                   "j"         #'p-org-presentation-on
         :desc "presentation mode off"                  "J"         #'p-org-presentation-off
         ;; flyspell
         :prefix ("w" . "flyspell")
         :desc "flyspell-buffer"                        "b"         #'flyspell-buffer
         :desc "evil-next-flyspell-error"               "n"         #'evil-next-flyspell-error
         :desc "evil-prev-flyspell-error"               "p"         #'evil-prev-flyspell-error
         ;; tag match
         :prefix ("s")
         :desc "org-tags-sparse-tree"                   "t"         #'org-tags-sparse-tree))
  ;; latex
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
               ;; beamer class, for presentations
               '("beamer"
                 "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"
                  "\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"))))


;; olivetti ------------------------------------------------------------------
(setq olivetti-body-width 0.7
      olivetti-minimum-body-width 80
      olivetti-recall-visual-line-mode-entry-state t)
