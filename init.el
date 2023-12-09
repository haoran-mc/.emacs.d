;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-

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
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc threshold to
;; temporarily prevent it from running, and then reset it by the `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; read-process-output-max: the maximum bytes read from processs in a single chunk (default is 4kb).
;; This is too small for the LSP protocol that uses JSON communication
(setq read-process-output-max (* 4 1024 1024))

(require 'package) ;; install package
(setq package-archives
      '(
        ("gnu"   . "http://1.15.88.122/gnu/")
        ("melpa" . "http://1.15.88.122/melpa/")

        ;; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
        ;; ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")

        ;; stop using it as wrong connect
        ;; ("melpa"  . "https://melpa.org/packages/")
        ;; ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             (cl-remove-if ;; 过滤不必要的目录
              #'(lambda (subdir)
                  (or
                   (not (file-directory-p (concat dir subdir))) ;; 移除非目录文件
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非 elisp 语言编写的 emacs 动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))
          (add-to-list 'load-path subdir-path t)) ;; `add-to-list' 第三个参数必须为 t，表示加到列表末尾 bfs
        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(let ((userdir (locate-user-emacs-file "lisp"))
      (extdir  (locate-user-emacs-file "site-lisp")))
  (add-subdirs-to-load-path (file-name-directory userdir))
  (add-subdirs-to-load-path (file-name-directory extdir)))
(setq custom-file (locate-user-emacs-file "custom.el"))


(require 'lazy-load)

(require 'init-base)
(require 'init-funcs)
(require 'init-utils)
(require 'init-ui)
(require 'init-tools)
;; (require 'init-evil)
(require 'init-lsp_bridge)
(require 'init-git)
(require 'init-dev)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-keybindings)

;; standalone apps
(require 'init-org)
(require 'init-font) ;; load(fira code) after org
(require 'init-site)
(require 'init-text)
(require 'init-reader)
(require 'init-eshell)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-osx))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
