;;; init-meow.el --- mode editing                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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

;;

;;; Code:

(require 'mwim)

(defun my-a-meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (when (and (< (point) (point-max))
                   (not (use-region-p))
                   (not (eolp))) ;; end of a line
          (forward-char 1))
      (meow--direction-forward)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun my-A-meow-append ()
  "Move to the end of line, switch to INSERT state."
  (interactive)
  (mwim-end-of-line)
  (meow--switch-state 'insert))

(defun my-C-meow-change ()
  "Kill till the end of line, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (progn
      (kill-line)
      (meow--switch-state 'insert))))

(defun my-F-meow-find (ch &optional expand)
  "Call `meow-find` with -1 and the given char."
  (interactive "cFind:")
  (meow-find -1 ch expand))

(defun my-I-meow-insert ()
  "Move to the beginning of code, switch to INSERT state."
  (interactive)
  (mwim-beginning-of-code)
  (meow--switch-state 'insert))

(defun my-r-meow-replace ()
  (interactive)
  (let ((mychar (read-char "replace with:")))
    (if (= ?\C-\[ mychar)
        (message "Cancelled replace")
      (delete-char 1)
      (insert mychar))))

(defun my-T-meow-till (ch &optional expand)
  "Call `meow-till` with -1 and the given char."
  (interactive "cTill:")
  (meow-till -1 ch expand))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   ;; '("j" . "H-j")
   ;; '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . mwim-beginning-of-line-or-code)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(":" . execute-extended-command)
   '(";" . repeat)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . my-a-meow-append)
   '("A" . my-A-meow-append)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . my-C-meow-change)
   '("d" . meow-kill) ;; ctrl+k vanilla/smart-kill-line
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . my-F-meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . my-I-meow-insert)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   ;; '("m" . meow-join) ;; C-j vanilla/merge-line-down
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . my-r-meow-replace)
   '("R" . meow-replace) ;; meow-swap-grap
   '("s" . meow-delete)
   '("t" . meow-till) ;; far and useless
   '("T" . my-T-meow-till)
   ;; '("u" . meow-undo) ;; just use C-/
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save) ;; M-w cancel the selection, but y not.
   ;; '("Y" . meow-sync-grab)
   '("z" . meow-reverse) ;; meow-pop-selection
   ;; '("'" . repeat)
   ;; '("/" . meow-visit)
   '("<escape>" . ignore)))

(require 'meow)
(meow-setup)

;; https://github.com/meow-edit/meow/issues/83#issuecomment-985490589
(define-key input-decode-map (kbd "C-[") [control-bracketleft])
(define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit)


(setq meow-use-clipboard t
      meow-char-thing-table '((?( . round)  ;; (
                                (?) . round)  ;; )
                              (?[ . square) ;; [
                                (?] . square) ;; ]
                              (?{ . curly)  ;; {
                              (?} . curly)  ;; }
                              (?g . string)
                              (?e . symbol)
                              (?w . window)
                              (?b . buffer)
                              (?p . paragraph)
                              (?l . line)
                              (?v . visual-line)
                              (?f . defun)
                              (?. . sentence))
      meow-expand-hint-counts '((word . 0)
                                (line . 0)
                                (block . 0)
                                (find . 0)
                                (till . 0)))

(setq meow-mode-state-list
      (append meow-mode-state-list
              '((lsp-bridge-ref-mode . insert)
                (magit-status-mode . insert))))


(meow-global-mode 1)


(require 'open-newline)
(defun meow-switch-insert-with-arg-advice (arg)
  "Enter insert mode after opening a new row."
  (meow--switch-state 'insert))

(advice-add 'open-newline-below :after 'meow-switch-insert-with-arg-advice)
(advice-add 'open-newline-above :after 'meow-switch-insert-with-arg-advice)
;; (advice-add 'set-mark-command :before 'meow-switch-insert-with-arg-advice)


;;;###autoload
(defun w ()
  "save current buffer (vim ':w' command)"
  (interactive)
  (save-buffer))


(provide 'init-meow)
;;; init-meow.el ends here
