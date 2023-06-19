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


(global-set-key (kbd "C-c i a") '+insert-github-io-images-dir-pwd)
(global-set-key (kbd "C-c i b") '+insert-export-images-dir-pwd)




(provide 'init-keybindings)
;;; init-keybindings.el ends here
