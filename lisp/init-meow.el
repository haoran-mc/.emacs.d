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

;;; Require:
(require 'mwim)

(defun my/meow-save-buffer-if-w ()
  "Prompt the user for a string and save the buffer if the string is 'w'."
  (interactive)
  (let ((input (read-string "Enter a string: ")))
    (when (string-equal input "w")
      (save-buffer)
      (message "Wrote %s" (buffer-file-name)))))

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

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("<escape>" . ignore))

  ;; No set Leader keys.
  (meow-leader-define-key)

  (meow-normal-define-key
   '(":" . my/meow-save-buffer-if-w)
   '(";" . repeat)
   '("?" . meow-cheatsheet)
   '("<escape>" . ignore)

   '("(" . whole-line-or-region-indent-rigidly-left-to-tab-stop)
   '(")" . whole-line-or-region-indent-rigidly-right-to-tab-stop)

   ;; movement, like hjkl
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)

   ;; insert
   '("a" . my-a-meow-append)
   '("A" . my-A-meow-append)
   '("i" . meow-insert)
   '("I" . my-I-meow-insert)

   ;; yank/pop
   ;; '("y" . meow-save) ;; M-w cancel the selection, but y not.
   ;; '("p" . meow-yank)

   ;; kill/delete/change/replace
   ;; '("d" . meow-kill) ;; C-w kill
   '("s" . meow-change)
   '("C" . my-C-meow-change)
   '("r" . my-r-meow-replace)
   '("R" . meow-replace) ;; meow-swap-grap
   '("x" . meow-delete)

   ;; find/till/visit/search, most used in beacon mode
   '("f" . my/avy-goto-char-in-line-after-point)
   '("F" . avy-goto-char-in-line)

   ;; mark
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("o" . meow-block)

   '("z" . (lambda () (interactive) (recenter-top-bottom)))

   ;; thing
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)))

(require 'meow)
(meow-setup)

;; https://github.com/meow-edit/meow/issues/83#issuecomment-985490589
(define-key input-decode-map (kbd "C-[") [control-bracketleft])
(define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit)


(meow-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))
(meow-thing-register 'backquote '(regexp "`" "`") '(regexp "`" "`"))

(setq meow-use-clipboard t
      meow-char-thing-table '((?\( . round)  ;; (
                              (?\) . round)  ;; )
                              (?\[ . square) ;; [
                              (?\] . square) ;; ]
                              (?{ . curly)   ;; {
                              (?} . curly)   ;; }
                              (?< . angle)   ;; }
                              (?> . angle)   ;; }
                              (?\" . string)
                              (?' . string)
                              (?` . backquote)
                              (?e . symbol)
                              (?w . window)
                              (?b . buffer)
                              (?p . paragraph)
                              (?l . line)
                              (?v . visual-line)
                              (?f . defun)
                              (?. . sentence))
      meow-expand-hint-counts '((word . 9)
                                (line . 9)
                                (block . 9)
                                (find . 9)
                                (till . 9))
      ;; meow-expand-hint-remove-delay 7
      meow-esc-delay 0.001)

(setq meow-mode-state-list
      (append meow-mode-state-list
              '((lsp-bridge-ref-mode . insert)
                (magit-status-mode . insert)
                (vundo-mode . insert)
                (messages-buffer-mode . normal)
                (eshell-mode . insert))))

(meow-global-mode 1)

(with-eval-after-load 'open-newline
  (defun meow-switch-insert-with-arg-advice (arg)
    "Enter insert mode after opening a new row."
    (meow--switch-state 'insert))

  (advice-add 'open-newline-below :after 'meow-switch-insert-with-arg-advice)
  (advice-add 'open-newline-above :after 'meow-switch-insert-with-arg-advice))

(with-eval-after-load 'hydra
  (defun meow-switch-insert-advice ()
    "Enter insert mode before hydra-rectangle/body."
    (meow--switch-state 'insert))

  (defun meow-switch-normal-advice ()
    "Enter normal mode before hydra-rectangle/body."
    (meow--switch-state 'normal))

  (advice-add 'hydra-rectangle/body :before 'meow-switch-insert-advice))


(provide 'init-meow)
;;; init-meow.el ends here
