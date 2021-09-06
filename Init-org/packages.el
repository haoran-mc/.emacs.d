(setq ogmc-org-packages
      '(
        org
        org-bullets
        )
      )

(use-package org
  :config
  (with-eval-after-load 'org
    (progn
      (setq org-startup-folded 'content);; 只显示标题
      )
    )
  )

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))
  :config
  (if (equal window-system 'x)
      (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))
  (setq org-bullets-bullet-list '("☯" "◉" "○" "✿" "❀" "◇"))
  )
