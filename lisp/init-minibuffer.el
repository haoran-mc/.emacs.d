;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

;; Completion engine
(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-local-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-ns-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-completion-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-must-match-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-isearch-map
              ([escape] . abort-recursive-edit))
  :custom
  ;; Default minibuffer is fine-tuned since Emacs 29
  (completion-auto-help t)
  (completion-show-help nil)
  (completion-cycle-threshold nil)
  (completion-auto-select 'second-tab)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode t)
  ;; Don't insert completion at point into minibuffer
  (minibuffer-completion-auto-choose nil)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; `selectrum', `vertico' and `icomplete' will honoring
  (completion-styles '(basic partial-completion substring flex))
  (completion-category-overrides '((buffer (styles . (flex)))))
  ;; vertical view
  (completions-format 'one-column)
  (completions-max-height 13)
  (completions-detailed t))

;; vertical style minibuffer
(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-cycle t)
  (vertico-sort-function nil))

;; support Pinyin first character match for orderless, avy etc.
(use-package pinyinlib
  :ensure t)

(use-package orderless
  :ensure t
  :config
  ;; make completion support pinyin, refer to
  ;; https://emacs-china.org/t/vertico/17913/2
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  :custom
  (completion-styles '(orderless)))

;; show description on minibuffer, like this:
;; lisp/      drwxr-xr-x    192  19 mis ago
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;; provides commands for finding and completing
(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap switch-to-buffer]       . consult-buffer)
         ([remap yank-pop]               . consult-yank-pop)
         ([remap apropos]                . consult-apropos)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap multi-occur]            . consult-multi-occur)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap isearch-forward]        . consult-line)
         ([remap projectile-ripgrep]     . consult-ripgrep)
         ([remap evil-show-marks]        . consult-mark)
         ("C-x j"                        . consult-mark)
         ([remap org-goto]               . consult-org-heading)
         :map org-mode-map
         ("C-c C-j"                      . consult-org-heading)
         :map prog-mode-map
         ("C-c C-j"                      . consult-outline))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))

  (with-eval-after-load 'embark
    (define-key embark-file-map (kbd "E") #'+consult-directory-externally))

  (defun +consult-directory-externally (file)
    "Open FILE externally using the default application of the system."
    (interactive "fOpen externally: ")
    (if (and (eq system-type 'windows-nt)
	         (fboundp 'w32-shell-execute))
        (shell-command-to-string
         (encode-coding-string
          (replace-regexp-in-string
           "/" "\\\\" (format "explorer.exe %s"
                              (file-name-directory
                               (expand-file-name file)))) 'gbk))
      (call-process (pcase system-type
		              ('darwin "open")
		              ('cygwin "cygstart")
		              (_ "xdg-open"))
		            nil 0 nil
		            (file-name-directory (expand-file-name file)))))

  (defun +open-current-directory ()
    "Open current FILE directory.
externally using the default application of the system."
    (interactive)
    (+consult-directory-externally default-directory))
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

(use-package prescient
  :ensure t
  :hook (after-init . prescient-persist-mode)
  :init
  (use-package vertico-prescient
    :ensure t
    :hook (vertico-mode . vertico-prescient-mode)
    :init
    (setq vertico-prescient-enable-filtering nil))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable nil))

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-o"     . embark-act)
              ("C-c C-c" . embark-export)
              ("C-c C-o" . embark-collect))
  :custom
  (prefix-help-command 'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
