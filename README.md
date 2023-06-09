<h3 align="center">A fast and concise Emacs config</h3>

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

<div align="center">

[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 27.1-29.x](https://img.shields.io/badge/Supports-Emacs_27.1_--_29.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

</div>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [个人**Emacs**配置](#个人emacs配置)
- [需要的依赖](#需要的依赖)
- [基础配置](#基础配置)
- [插件配置、升级](#插件配置升级)
- [按键绑定](#按键绑定)
- [prog-mode](#prog-mode)
- [Emacs 最小配置](#emacs-最小配置)

<!-- markdown-toc end -->

个人**Emacs**配置
====

改 [condy0919/.emacs.d](https://github.com/condy0919/.emacs.d) 的个人配置。

```bash
git clone --depth 1 https://github.com/haoran-mc/.emacs.d ~/.emacs.d
```

# 需要的依赖

- [ripgrep](https://github.com/BurntSushi/ripgrep)：更快的 `grep`；
- [git](https://git-scm.com/)：分布式版本控制工具；
- [lsp-bridge](https://github.com/manateelazycat/lsp-bridge/blob/master/README.zh-CN.md)；
  - macOS 用户需要给 brew 命令增加选项 `--with-rsvg` 来安装 Emacs 才能显示 SVG 图片；
- [grip](https://github.com/joeyespo/grip)：预览 markdown，`brew install grip`；
- [all-the-icons](https://github.com/domtronn/all-the-icons.el)：`M-x all-the-icons-install-fonts`；

# 基础配置

最基础的配置包含了那些在所有 `mode` 下都不会变更的配置，包含了：

| 包名          | 功能                                         |
|---------------|----------------------------------------------|
| hl-line       | 高亮当前行                                   |
| newcomment    | 注释、反注释功能                             |
| paren         | 高亮匹配的括号                               |
| saveplace     | 自动记录上次打开文件的位置                   |
| simple        | 在modeline里显示行号、列号以及当前文本的大小 |
| so-long       | 打开长行的文件不再痛苦 (Emacs 27+ 自带)      |
| tab-bar       | 窗口布局管理 (Emacs 27+ 自带)                |

而这几个包也是 Emacs 自带的。

为了保持界面的整洁，禁用了菜单栏、工具栏和滚动条。

更多的基础配置看 [lisp/init-base.el](https://github.com/haoran-mc/.emacs.d/blob/main/lisp/init-base.el) 文件。

# 插件配置、升级

`package.el` （自带的）来安装包、`use-package` 来管理配置。对于 `elpa`、`melpa` 里没有的包，使用 `quelpa` 辅助下载。

Emacs 29 引入了 `package-update-all`，需要更新直接 `M-x package-update-all` 即可。

# 按键绑定

- [如何管理我的按键绑定](https://github.com/haoran-mc/.emacs.d/blob/main/docs/master-keybindings.md)
- [系统自带的按键与配置的按键](https://github.com/haoran-mc/.emacs.d/blob/main/docs/shortcuts.md)

# prog-mode

仅配置了 python、elisp 的编程环境，更多的代码编辑使用 neovim。

补全、符号查找的工具使用 `lsp-bridge`。

# Emacs 最小配置

当 Emacs 配置出问题时，可以使用 [init-mini.el](https://github.com/haoran-mc/.emacs.d/blob/main/init-mini.el) 这个最小配置来临时救急一下。

``` bash
emacs -Q -l init-mini.el
```
