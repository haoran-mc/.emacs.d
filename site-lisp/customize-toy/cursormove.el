


;;;###autoload
(defun vanilla/move-to-window-top ()
  "Move the current line and cursor to the top third of the window."
  (interactive)
  (recenter-top-bottom 0))

;;;###autoload
(defun vanilla/move-to-window-middle ()
  "Move the current line and cursor to the middle of the window."
  (interactive)
  (recenter-top-bottom))

;;;###autoload
(defun vanilla/move-to-window-bottom ()
  "Move the current line and cursor to the bottom third of the window."
  (interactive)
  (recenter-top-bottom -1))

;;;###autoload
(defun lazycat/scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun lazycat/scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun vanilla/scroll-left-half-page ()
  "Scroll the window left by half the page height."
  (interactive)
  (scroll-left (/ (window-body-height) 2)))

(defun vanilla/scroll-right-half-page ()
  "Scroll the window right by half the page height."
  (interactive)
  (scroll-right (/ (window-body-height) 2)))

(defun vanilla/move-cursor-8-lines-down ()
  "Move the cursor down by 8 lines."
  (interactive)
  (next-line 8))

(defun vanilla/move-cursor-8-lines-up ()
  "Move the cursor up by 8 lines."
  (interactive)
  (previous-line 8))

;; (global-set-key (kbd "M-n") 'move-cursor-8-lines-down)
;; (global-set-key (kbd "M-p") 'move-cursor-8-lines-up)


(provide 'cursormove)
;;; cursormove.el
