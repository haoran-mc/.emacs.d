;;; init-ui.el --- Basic configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(column-number-mode 1)

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(global-linum-mode t);;显示行号
(setq linum-format "   ");;自定义行号格式
(setq-default frame-title-format "Forming into my Dreamtale!");;设置标题
(setq initial-frame-alist (quote ((fullscreen . maximized))));;开启全屏
(global-hl-line-mode t);;高亮当前行
(setq ring-bell-function 'ignore);;关闭警报
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas")
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 10"))
(setq-default inhibit-startup-screen t);;不显示欢迎页面
;; (setq-default cursor-type 'bar);;设置光标形状为竖线（默认为方块）


;; (set-frame-parameter (selected-frame) 'alpha (list 85 60))
;; (add-to-list 'default-frame-alist (cons 'alpha (list 85 60)))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))


;; ;;配色方案
;; (setq default-frame-alist
;;       '((vertical-scroll-bars)
;;         (background-color . "grey15")
;;         (foreground-color . "grey")
;;         (cursor-color . "gold1")
;;         (mouse-color . "gold1")
;;         ; (top . 25)
;;         ; (left . 45)
;;         ; (width . 120)
;;         ; (height . 40)
;;         ; (tool-bar-lines . 0)
;;         ; (menu-bar-lines . 1)
;;         ; (scroll-bar-lines . 0)
;;         (right-fringe)
;;         (left-fringe)))
;; 
;; (set-face-background 'highlight "gray5")
;; (set-face-foreground 'region "cyan")
;; (set-face-background 'region "blue")
;; (set-face-foreground 'secondary-selection "skyblue")
;; (set-face-background 'secondary-selection "darkblue")
;; (set-cursor-color "wheat")
;; (set-mouse-color "wheat")

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
  (setq mode-line-misc-info (cdr mode-line-misc-info)))

(ogmc-ui/init-ogmc-mode-line)

(use-package ogmc-mode-line
  ;; "doom-modeline"
  :hook
  (add-hook 'after-init-hook #'doom-modeline-mode)
  :config
  (setq doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-unicode-fallback nil
        doom-modeline-mu4e nil
        doom-modeline-icon nil
        )
  (unless after-init-time
    (setq doom-modeline--default-format mode-line-format)
    (setq-default mode-line-format nil)))


(use-package dashboard
  :init
  (defcustom centaur-logo (expand-file-name
                           (if (display-graphic-p) "~/.emacs.d/dashboard/logo6.png" "~/.emacs.d/dashboard/logo6.txt")
                           user-emacs-directory)
    "Set Centaur logo. nil means official logo."
    :group 'centaur
    :type 'string)
  :config
  (progn
    (dashboard-setup-startup-hook)

    (if (equal window-system 'x)
        (setq dashboard-center-content nil
              dashboard-footer (format "Powered by Haoran Lorangez, %s" (format-time-string "%Y")))
      (setq dashboard-center-content nil
            dashboard-footer-messages '("")
            dashboard-items '((bookmarks . 8)(recents  . 20))))

    (setq dashboard-center-content nil ;;不放在中间
          dashboard-set-init-info nil  ;;开启的信息
          dashboard-set-navigator nil  ;;导航
          dashboard-set-footer nil     ;;不设置底部
          dashboard-banner-logo-title "A sailing ship to a Distant horizon."
          dashboard-startup-banner (or centaur-logo 'official)
          dashboard-show-shortcuts nil
          dashboard-set-file-icons nil
          dashboard-set-heading-icons nil)))


(provide 'init-ui)
;;; init-ui.el ends here
