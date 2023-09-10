;;; init-keybindings.el --- keys -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;;

;;; Code:

(global-set-key (kbd "M-:") 'execute-extended-command)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "C-c u f") '+unfill-paragraph)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i l") #'+dwim-create-link-with-datetime)
  (define-key org-mode-map (kbd "C-c i i") #'+org-insert-image)
  (define-key org-mode-map (kbd "C-c i y") #'yas-insert-snippet)
  )

(global-set-key (kbd "C-c i t") 'hl-todo-insert)


(global-set-key (kbd "C-c s") 'tab-bar-switch-to-tab)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
