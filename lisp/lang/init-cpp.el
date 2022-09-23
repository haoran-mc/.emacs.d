;;; init-cpp.el --- Cpp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c C-c" . +compile-file)
              ("<f9>"    . +run-file)
              ("<f10>"   . gud-gdb))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4)
  :config
  (defun +compile-file ()
    (interactive)
    (compile
     (format "g++ -o %s %s -g -lm -Wall"
             (file-name-sans-extension (buffer-name))
             (buffer-name))))

  (defun +run-file ()
    (interactive)
    (if (with-no-warnings (eshell-command
           (format "g++ -o a %s -g -lm -Wall"
                   (buffer-name))))
        (aweshell-dedicated-toggle)))

  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))

(provide 'init-cpp)

;;; init-cpp.el ends here
