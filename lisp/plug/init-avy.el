;;; init-avy.el --- quick move -*- lexical-binding: t; -*-

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


(provide 'init-avy)
;;; init-avy.el ends here
