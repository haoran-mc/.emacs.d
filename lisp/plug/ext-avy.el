;;; ext-avy.el --- quick move -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;; use evil-avy-... firstly as we use evil

;;; Code:



;; Jump to arbitrary positions
(use-package avy
  :ensure t
  ;; integrate with isearch and others
  ;; C-' to select isearch-candidate with avy
  :hook (after-init . avy-setup-default)
  :config
  (when (>= emacs-major-version 28)
    (use-package transient
      :ensure nil
      :config
      (transient-define-prefix avy-menu ()
        "Avy quick menu."
        :transient-suffix     'transient--do-stay
        :transient-non-suffix 'transient--do-warn
        [["Move"
          ("j" "avy-next" avy-next)
          ("k" "avy-prev" avy-prev)
          ("p" "avy-pop-mark" avy-pop-mark)]
         ["Resume"
          ("r" "avy-resume" avy-resume)]
         ["Exit"
          ("q" "quit" transient-quit-one)]])))
  :custom
  (avy-background t)
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (avy-styles-alist '((avy-isearch . pre))))


(provide 'ext-avy)
;;; ext-avy.el ends here
