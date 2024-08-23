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


;;; Code:
(global-unset-key (kbd "C-SPC"))

(defun my-DEL-meow-delete (N)
  "Delete region when region is active, otherwise delete N characters.
If `meow--temp-normal` is non-nil, switch to motion state first."
  (interactive "p")  ;; "p" means to read a prefix argument (N)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (backward-delete-char-untabify 1))))
(global-set-key (kbd "DEL") #'my-DEL-meow-delete)

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

(defun my-v-meow-expand ()
  "Activate char selection, then move right."
  (interactive)
  (if (region-active-p)
      (thread-first
        (meow--make-selection '(expand . char) (mark) (point))
        (meow--select))
    (thread-first
      (meow--make-selection '(expand . char) (point) (point))
      (meow--select))))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("<escape>" . ignore))

  ;; No set Leader keys.
  (meow-leader-define-key)

  (meow-normal-define-key
   ;; expand by numbers
   '("0" . meow-expand-0)
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
   '(":" . my/meow-save-buffer-if-w)
   '(";" . repeat)
   '("?" . meow-cheatsheet)

   '("(" . whole-line-or-region-indent-rigidly-left-to-tab-stop)
   '(")" . whole-line-or-region-indent-rigidly-right-to-tab-stop)

   ;; movement, like hjkl
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("H" . meow-left-expand)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)

   ;; insert
   '("a" . my-a-meow-append)
   '("A" . my-A-meow-append)
   '("i" . meow-insert)
   '("I" . my-I-meow-insert)

   ;; yank/pop
   '("y" . meow-save) ;; M-w cancel the selection, but y not.
   '("p" . meow-yank)

   ;; kill/delete/change/replace
   '("d" . meow-kill) ;; C-w kill
   '("D" . meow-kill-whole-line) ;; C-w whole-line-or-region-kill-region
   '("s" . meow-delete)
   '("c" . meow-change)
   '("C" . my-C-meow-change)
   '("r" . my-r-meow-replace)
   '("R" . meow-replace) ;; meow-swap-grap

   ;; find/till/visit/search, most used in beacon mode
   '("f" . meow-find)
   '("F" . my-F-meow-find)
   '("t" . meow-till) ;; far and useless
   '("T" . my-T-meow-till)
   '("/" . meow-visit)
   '("n" . meow-search)

   ;; mark
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("v" . my-v-meow-expand)

   ;; thing
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)

   ;; grab [TODO]
   '("G" . meow-grab)
   ;; '("Y" . meow-sync-grab)

   ;; '("u" . meow-undo) ;; just use C-/
   ;; '("U" . meow-undo-in-selection)
   ;; '("m" . meow-join) ;; C-j vanilla/merge-line-down
   '("g" . meow-cancel-selection)
   '("q" . meow-quit)
   '("z" . meow-reverse) ;; meow-pop-selection
   '("<escape>" . ignore)))

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
      meow-expand-hint-remove-delay 7
      meow-esc-delay 0.001)

(setq meow-mode-state-list
      (append meow-mode-state-list
              '((lsp-bridge-ref-mode . insert)
                (magit-status-mode . insert)
                (vundo-mode . insert)
                (messages-buffer-mode . normal)
                (eshell-mode . insert))))

(meow-global-mode 1)


(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c 0" "digit-arg")
  (which-key-add-key-based-replacements "C-c 1" "digit-arg")
  (which-key-add-key-based-replacements "C-c 2" "digit-arg")
  (which-key-add-key-based-replacements "C-c 3" "digit-arg")
  (which-key-add-key-based-replacements "C-c 4" "digit-arg")
  (which-key-add-key-based-replacements "C-c 5" "digit-arg")
  (which-key-add-key-based-replacements "C-c 6" "digit-arg")
  (which-key-add-key-based-replacements "C-c 7" "digit-arg")
  (which-key-add-key-based-replacements "C-c 8" "digit-arg")
  (which-key-add-key-based-replacements "C-c 9" "digit-arg"))

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
