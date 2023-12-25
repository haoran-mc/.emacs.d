;;; init-keyfreq.el ---how many times you used a command  -*- lexical-binding: t; -*-

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

;; https://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html

;;; Code:


(require 'keyfreq)


;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-keyfreq.el
(defun turnon-keyfreq-mode ()
  "Turn on keyfreq."
  (interactive)
  ;; Fire up keyfreq a few seconds later to start up emacs faster
  (my-run-with-idle-timer 4 (lambda ()
                              (keyfreq-mode 1)
                              (keyfreq-autosave-mode 1))))



(with-eval-after-load 'keyfreq
  (setq keyfreq-excluded-commands
        '(self-insert-command
          mac-mwheel-scroll
          undefined))
  (setq keyfreq-excluded-regexp
        '()))


(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
