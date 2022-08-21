;;; init-modeline.el --- a custom modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2022  HaoRan Liu

;; Author: HaoRan Liu <haoran.mc@gmail.com>
;; Keywords: faces

;;; Commentary:

;;; Code:

(require 'flycheck)

(defvar spacemacs--indent-variable-alist
  ;; Note that derived modes must come before their sources
  '(((awk-mode c-mode c++-mode java-mode
               idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (groovy-mode . groovy-indent-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (rust-mode . rust-indent-offset)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (typescript-mode . typescript-indent-level)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is.
either a symbol corresponding to a major mode, a list of
such symbols, or the symbol t, acting as default. The
values are either integers, symbol sor lists of these.")


(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun ogmc/display-mode-indent-width ()
  "Display indent width."
  (let ((mode-indent-level
         (catch 'break
           (dolist (test spacemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (concat "TS:" (int-to-string (or mode-indent-level 0)))))

(defvar my-flycheck-mode-line
  '(:eval
    (when
        (and (bound-and-true-p flycheck-mode)
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
      (pcase flycheck-last-status-change
        ((\` not-checked) nil)
        ((\` no-checker) (propertize " -" 'face 'warning))
        ((\` running) (propertize " ✷" 'face 'success))
        ((\` errored) (propertize " !" 'face 'error))
        ((\` finished)
         (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                (no-errors (cdr (assq 'error error-counts)))
                (no-warnings (cdr (assq 'warning error-counts)))
                (face (cond (no-errors 'error)
                            (no-warnings 'warning)
                            (t 'success))))
           (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                       'face face)))
        ((\` interrupted) " -")
        ((\` suspicious) '(propertize " ?" 'face 'warning))))))

(setq-default mode-line-misc-info (assq-delete-all 'which-func-mode mode-line-misc-info))

;; normal filename.el                                                  (107:71) (22:52)

(setq-default mode-line-format
              (list
               ;; evil state
               '(:eval evil-mode-line-tag)

               "%1 "
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))


               " [" ;; insert vs overwrite mode, input-method in a tooltp
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-builtin-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode
                                                          "overwrite"
                                                        "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ", " (propertize "Mod"
                                                  'face 'font-lock-builtin-face
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ", " (propertize "RO"
                                                  'face 'font-lock-builtin-face
                                                  'help-echo "Buffer is read-only"))))
               "] "
               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-builtin-face) ;; % above top
               "] "

               ;; the current major mode for the buffer.
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))

               ;; ;; git info
               ;; '(:eval (when (> (window-width) 90)
               ;;           `(vc-mode vc-mode)))
               ;; " "

               ;; ;; global-mode-string goes in mode-line-misc-info
               ;; '(:eval (when (> (window-width) 120)
               ;;           mode-line-misc-info))

               ;; fill for set modeline tail
               (mode-line-fill 'mode-line 25)

               ;; '(:eval (ogmc/display-mode-indent-width))

               "%1 "
               my-flycheck-mode-line
               "%1 "

               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%l" 'face 'font-lock-constant-face) ", "
               (propertize "%c" 'face 'font-lock-constant-face)
               ") "

               ;; '(:eval (when (> (window-width) 80)
               ;;           (buffer-encoding-abbrev)))
               mode-line-end-spaces
               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "(%H:%M)")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               ))


(setq mode-line-misc-info (cdr mode-line-misc-info))

(provide 'init-modeline)
;;; init-modeline.el ends here
