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


(setq minibuffer-eldef-shorten-default t) ;; shorten "(default ...)" to "[...]"


;; Persist history over Emacs restarts. Vertico sorts by history position.
(require 'savehist)
(savehist-mode 1)
(setq history-length 1000)


;; vertico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertical style minibuffer
(require 'vertico)
(add-hook 'after-init-hook 'vertico-mode)
(add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
(setq vertico-cycle t)
;; extensions
(require 'vertico-repeat)


;; orderless ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'orderless)
;; make completion support pinyin, refer to
;; https://emacs-china.org/t/vertico/17913/2
(defun completion--regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
(setq completion-styles '(orderless))


;; marginalia ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show description on minibuffer, like this:
;; lisp/      drwxr-xr-x    192  19 mis ago
(with-eval-after-load 'vertico
  (require 'marginalia)
  (add-hook 'vertico-mode-hook 'marginalia-mode))


;; pingyinlib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support Pinyin first character match for orderless, avy etc.
(require 'pinyinlib)




;; consult ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provides commands for finding and completing
;; lazy load by key
;; consult-ripgrep
;; consult-imenu
;; consult-goto-line
;; consult-goto-buffer
;; consult-yank-pop
;; consult-apropos
;; consult-bookmark
;; consult-recent-file
;; consult-multi-occur
;; consult-complex-command
;; consult-line
;; consult-mark
;; consult-org-heading
;; consult-line
(with-eval-after-load 'consult
  (define-key prog-mode-map (kbd "C-c C-j") 'consult-outline)
  (with-eval-after-load 'org
    (define-key org-mode-map [remap org-goto] 'consult-org-heading))

  (with-no-warnings
    (consult-customize consult-git-grep consult-grep ;; consult-ripgrep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))

  (setq consult-fontify-preserve nil
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))


;; (with-eval-after-load 'embark
;;   (define-key embark-file-map (kbd "E") #'+consult-directory-externally))
;;
;; (defun +consult-directory-externally (file)
;;   "Open FILE externally using the default application of the system."
;;   (interactive "fOpen externally: ")
;;   (if (and (eq system-type 'windows-nt)
;; 	       (fboundp 'w32-shell-execute))
;;       (shell-command-to-string
;;        (encode-coding-string
;;         (replace-regexp-in-string
;;          "/" "\\\\" (format "explorer.exe %s"
;;                             (file-name-directory
;;                              (expand-file-name file)))) 'gbk))
;;     (call-process (pcase system-type
;; 		            ('darwin "open")
;; 		            ('cygwin "cygstart")
;; 		            (_ "xdg-open"))
;; 		          nil 0 nil
;; 		          (file-name-directory (expand-file-name file)))))
;;
;; (defun +open-current-directory ()
;;   "Open current FILE directory.
;; externally using the default application of the system."
;;   (interactive)
;;   (+consult-directory-externally default-directory))




;; (use-package prescient
;;   :ensure t
;;   :hook (after-init . prescient-persist-mode)
;;   :init
;;   (use-package vertico-prescient
;;     :ensure t
;;     :hook (vertico-mode . vertico-prescient-mode)
;;     :init
;;     (setq vertico-prescient-enable-filtering nil))
;;   :config
;;   (setq prescient-sort-full-matches-first t
;;         prescient-sort-length-enable nil))
;;
;; (use-package embark
;;   :ensure t
;;   :bind (:map minibuffer-local-map
;;               ("M-o"     . embark-act)
;;               ("C-c C-c" . embark-export)
;;               ("C-c C-o" . embark-collect))
;;   :custom
;;   (prefix-help-command 'embark-prefix-help-command))
;;
;; ;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
