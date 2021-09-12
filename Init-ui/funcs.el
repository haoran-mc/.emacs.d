(defun ogmc-ui/init-ogmc-mode-line ()
  (defun ogmc/display-mode-indent-width ()
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

  (setq my-flycheck-mode-line
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

  (setq-default mode-line-format
                (list
                 '(:eval evil-mode-line-tag)
                 "["
                 '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                     'face 'font-lock-builtin-face
                                     'help-echo (concat "Buffer is in "
                                                        (if overwrite-mode
                                                            "overwrite"
                                                          "insert") " mode")))
                 '(:eval (when (buffer-modified-p)
                           (concat "-" (propertize "Mod"
                                                   'face 'font-lock-builtin-face
                                                   'help-echo "Buffer has been modified"))))
                 "]"
                 "%1 "
                 '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                     'help-echo (buffer-file-name)))
                 '(:eval (propertize "%m" 'face 'font-lock-string-face
                                     'help-echo buffer-file-coding-system))
                 "%1"
                 my-flycheck-mode-line
                 "%1"
                 " ["
                 (propertize "%p" 'face 'font-lock-builtin-face)
                 "] "
                 "["
                 (propertize "%l" 'face 'font-lock-constant-face) "-"
                 (propertize "%c" 'face 'font-lock-constant-face)
                 "]"
                 ))
  (setq mode-line-misc-info (cdr mode-line-misc-info))
  )
