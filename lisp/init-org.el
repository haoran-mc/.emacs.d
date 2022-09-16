;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)
(require 'evil)

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . (lambda()
                       (define-key evil-motion-state-map (kbd "RET") 'org-open-at-point)
                       (define-key evil-motion-state-map (kbd "C-c &") 'org-mark-ring-goto)
                       (message "hello world"))))
  :init
  (require 'org-tempo) ;; <s
  :custom
  (org-directory "~/haoran/Notes/Org/org-directory")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  ;; prettify
  (org-startup-indented t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  ;; image
  (org-image-actual-width nil)
  (org-display-remote-inline-images 'cache)
  ;; more user-friendly
  (org-imenu-depth 4)
  (org-clone-delete-id t)
  (org-use-sub-superscripts '{})
  (org-yank-adjusted-subtrees t)
  (org-ctrl-k-protect-subtree 'error)
  (org-catch-invisible-edits 'smart)
  (org-insert-heading-respect-content t)
  ;; call C-c C-o explicitly
  (org-return-follows-link nil)
  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                       (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WIP"        :foreground "#0098dd" :weight bold)
                            ("WAIT"       :foreground "#9f7efe" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)
                            ("REPORT"     :foreground "magenta" :weight bold)
                            ("BUG"        :foreground "red"     :weight bold)
                            ("KNOWNCAUSE" :foreground "yellow"  :weight bold)
                            ("FIXED"      :foreground "green"   :weight bold)))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-priority-faces '((?A :foreground "red")
                        (?B :foreground "orange")
                        (?C :foreground "yellow")))
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("STYLE_ALL" . "habit")))
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; Remove CLOSED: [timestamp] after switching to non-DONE states
  (org-closed-keep-when-no-todo t)
  ;; log
  (org-log-repeat 'time)
  (org-log-into-drawer t)
  ;; refile
  (org-refile-use-cache nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; goto. We use minibuffer to filter instead of isearch.
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-tag-inheritance nil)
  (org-agenda-use-tag-inheritance nil)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  ;; archive
  (org-archive-location "%s_archive::datetree/")
  ;; id
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; abbreviation for url
  (org-link-abbrev-alist '(("GitHub" . "https://github.com/")
                           ("GitLab" . "https://gitlab.com/")
                           ("Google" . "https://google.com/search?q=")
                           ("RFCs"   . "https://tools.ietf.org/html/")
                           ("LWN"    . "https://lwn.net/Articles/")
                           ("WG21"   . "https://wg21.link/")))
  ;; Open link in full screen
  (org-link-frame-setup
   '((vm . vm-visit-folder)
     (vm-imap . vm-visit-imap-folder)
     (gnus . gnus)
     (file . find-file)
     (wl . wl-frame)))
  (org-M-RET-may-split-line '((header-line . nil)))
  (org-startup-folded 'content)
  (org-hide-block-startup t)
  (org-html-text-markup-alist
   '((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<verbatim>%s</verbatim>")))
  (org-level-faces '((org-level-1 :inherit outline-1 :height 1.0)
                     (org-level-2 :inherit outline-2 :height 1.0)
                     (org-level-3 :inherit outline-3 :height 1.0)
                     (org-level-4 :inherit outline-4 :height 1.0)
                     (org-level-5 :inherit outline-5 :height 1.0)
                     (org-level-6 :inherit outline-6 :height 1.0)
                     (org-level-7 :inherit outline-7 :height 1.0)
                     (org-level-8 :inherit outline-8 :height 1.0))))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :config
  ;; update appt list every 5 minutes
  (run-at-time t 300 #'org-agenda-to-appt)
  (shut-up! #'org-agenda-to-appt)
  :custom
  (org-agenda-files '("~/haoran/Notes/Org/agenda/"))
  (org-agenda-diary-file '("~/haoran/Notes/Org/diary/diary.org"))
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-inhibit-startup t)
  (org-agenda-time-leading-zero t)
  (org-agenda-remove-tags t)
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
              ;; consistent with separedit/magit
              ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t) ;; syntax code on org mode
  (org-edit-src-content-indentation 2) ;; src content indent 2
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation nil) ;; used with the sentence above
  (org-src-window-setup 'other-window)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)
                        ("go"     . go)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t)
                              (go         . t))))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . org-capture-setup)
  :config
  (with-no-warnings
    (defun org-capture-setup ()
      (setq-local org-complete-tags-always-offer-all-agenda-tags t))

    (defun project-todo-org-file (headline)
      (let* ((file (expand-file-name "TODO.org" (projectile-acquire-root)))
             (buf (find-file-noselect file)))
        (set-buffer buf)
        ;; Set to UTF-8 because we may be visiting raw file.
        (setq buffer-file-coding-system 'utf-8-unix)
        (unless (org-find-exact-headline-in-buffer headline)
          (goto-char (point-max))
          (insert "* " headline)
          (org-set-tags (downcase headline))))))
  :custom
  (org-capture-use-agenda-date t)
  (org-capture-templates-contexts nil)
  (org-capture-templates `(;; Tasks
                           ("t" "Tasks")
                           ("tt" "Today" entry (file+olp+datetree "tasks.org")
                            "* %? %^{EFFORT}p"
                            :prepend t)
                           ("ti" "Inbox" entry (file+headline "tasks.org" "Inbox")
                            "* %?\n%i\n")
                           ("tm" "Mail" entry (file+headline "tasks.org" "Inbox")
                            "* TODO %^{type|reply to|contact} %^{recipient} about %^{subject} :MAIL:\n")
                           ;; Capture
                           ("c" "Capture")
                           ("cn" "Note" entry (file+headline "capture.org" "Notes")
                            "* %? %^g\n%i\n")
                           ;; Project
                           ("p" "Project")
                           ("pb" "Bug"           entry (function ,(lazy! (project-todo-org-file "Bugs")))          "* %?")
                           ("pf" "Feature"       entry (function ,(lazy! (project-todo-org-file "Features")))      "* %?")
                           ("ps" "Security"      entry (function ,(lazy! (project-todo-org-file "Security")))      "* %?")
                           ("pe" "Enhancement"   entry (function ,(lazy! (project-todo-org-file "Enhancements")))  "* %?")
                           ("po" "Optimization"  entry (function ,(lazy! (project-todo-org-file "Optimizations"))) "* %?")
                           ("pd" "Documentation" entry (function ,(lazy! (project-todo-org-file "Documentation"))) "* %?")
                           ("pm" "Miscellaneous" entry (function ,(lazy! (project-todo-org-file "Miscellaneous"))) "* %?"))))

;; beautify org star
(use-package org-superstar
  :ensure nil
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "■" "◆" "▲" "▶"))
  ;; (setq org-superstar-bullet-list '("☰" "☷" "■" "◆" "▲" "▶"))
  (org-ellipsis " ▼ "))

(provide 'init-org)
;;; init-org.el ends here
