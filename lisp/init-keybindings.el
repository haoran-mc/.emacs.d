;;; init-keybindings.el --- keys -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;;

;;; Code:

(global-set-key (kbd "M-:") 'execute-extended-command)

;; h for help
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)


;; here is C-c ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check ext-which-key.el for prompt

;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(global-set-key (kbd "C-c c b") 'literate-calc-eval-buffer)
(global-set-key (kbd "C-c c i") 'literate-calc-insert-results)
(global-set-key (kbd "C-c c m") 'literate-calc-minor-mode)
(global-set-key (kbd "C-c c l") 'literate-calc-eval-line)
(global-set-key (kbd "C-c c c") 'literate-calc-clear-overlays)


;; i for insert
(global-set-key (kbd "C-c i t") 'hl-todo-insert)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i l") #'+dwim-create-link-with-datetime)
  (define-key org-mode-map (kbd "C-c i i") #'+org-insert-image)
  (define-key org-mode-map (kbd "C-c i y") #'yas-insert-snippet)
  (define-key org-mode-map (kbd "C-c i ,") #'org-insert-structure-template)
  (define-key org-mode-map (kbd "C-c i !") #'(lambda () (interactive) (org-time-stamp-inactive '(16)))))


;; n for narrow
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n s") #'org-narrow-to-subtree)
  (define-key org-mode-map (kbd "C-c n w") #'widen))


;; s for switch
(global-set-key (kbd "C-c s") 'tab-bar-switch-to-tab)

;; u for user
(global-set-key (kbd "C-c u f") '+unfill-paragraph)


(provide 'init-keybindings)
;;; init-keybindings.el ends here
