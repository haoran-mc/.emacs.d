
;;; init-company.el --- Company code -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; the completion engine
;;
;; 'company-mode' has an online manual now.
;;
;; https://company-mode.github.io/manual/
(use-package company
  :ensure nil
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete)
              :map company-active-map
              ("C-s"     . company-filter-candidates)
              ([tab]     . company-complete-common-or-cycle)
              ([backtab] . company-select-previous-or-abort))
  :config
  (define-advice company-capf--candidates (:around (func &rest args))
    "Try default completion styles."
    (let ((completion-styles '(basic partial-completion)))
      (apply func args)))
  :custom
  (company-idle-delay 0)
  ;; Easy navigation to candidates with M-<n>
  (company-show-quick-access t)
  (company-require-match nil)
  (company-minimum-prefix-length 3)
  (company-tooltip-width-grow-only t)
  (company-tooltip-align-annotations t)
  ;; complete `abbrev' only in current buffer and make dabbrev case-sensitive
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  ;; make dabbrev-code case-sensitive
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-code-everywhere t)
  ;; call `tempo-expand-if-complete' after completion
  (company-tempo-expand t)
  ;; Ignore uninteresting files. Items end with a slash are recognized as
  ;; directories.
  (company-files-exclusions '(".git/" ".DS_Store"))
  ;; No icons
  (company-format-margin-function nil)
  (company-backends '((company-capf :with company-tempo)
                      company-files
                      (company-dabbrev-code company-keywords)
                      company-dabbrev
                      ;; HACK: prevent `lsp-mode' to add `company-capf' back.
                      company-capf)))


(provide 'init-company)
;;; init-company.el ends here