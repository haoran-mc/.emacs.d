;;; ext-eaf.el --- emacs application frameword       -*- lexical-binding: t; -*-

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

;; No

;;; Code:


;; LOCAL-PACKAGES
(add-to-list 'load-path "~/Documents/emacs/local-packages/emacs-application-framework")
(require 'eaf)
(require 'eaf-rss-reader)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)

;; (add-hook 'eaf-mode-hook #'turn-off-evil-mode t)

;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
(setq eaf-python-command "python3")
(setq eaf-buffer-background-color "#282C34")
(setq eaf-start-python-process-when-require nil)

(setq eaf-browser-continue-where-left-off t)
(setq eaf-browser-enable-adblocker t)

(defalias 'browse-web #'eaf-open-browser)
(eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
(eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)

(eaf-bind-key eaf-rss-reader-scroll-up-web-page "SPC" eaf-rss-reader-keybinding)
(eaf-bind-key eaf-rss-reader-scroll-down-web-page "b" eaf-rss-reader-keybinding)


(provide 'ext-eaf)
;;; ext-eaf.el ends here
