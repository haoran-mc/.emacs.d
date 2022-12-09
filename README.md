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
    - [golang-mode](#golang-mode)
- [个人文件快速打开](#个人文件快速打开)
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
- [golang](https://go.dev)：编程语言；
  - gopls
  - goimports
  - guru...
- [python](https://www.python.org/)：编程语言；
  - python -m venv ENV_DIR
  - npm install -g pyright
- [lsp-bridge](https://github.com/manateelazycat/lsp-bridge/blob/master/README.zh-CN.md)；
- [grip](https://github.com/joeyespo/grip)：预览 markdown，`brew install grip`；
- [all-the-icons](https://github.com/domtronn/all-the-icons.el)：`M-x all-the-icons-install-fonts`；

# 基础配置

最基础的配置包含了那些在所有 `mode` 下都不会变更的配置，包含了：

| 包名          | 功能                                         |
|---------------|----------------------------------------------|
| appt          | 任务提醒，可以与org-mode结合                 |
| hippie-expand | 用来展开文本                                 |
| hl-line       | 高亮当前行                                   |
| newcomment    | 注释、反注释功能                             |
| paren         | 高亮匹配的括号                               |
| saveplace     | 自动记录上次打开文件的位置                   |
| simple        | 在modeline里显示行号、列号以及当前文本的大小 |
| so-long       | 打开长行的文件不再痛苦 (Emacs 27+ 自带)      |
| tab-bar       | 窗口布局管理 (Emacs 27+ 自带)                |
| tramp         | 远程编辑就靠它                               |

而这几个包也是 Emacs 自带的。

为了保持界面的整洁，禁用了菜单栏、工具栏和滚动条。

# 插件配置、升级

`package.el` （自带的）来安装包、`use-package` 来管理配置。对于 `elpa`、`melpa` 里没有的包，使用 `quelpa` 辅助下载。

Emacs 29 引入了 `package-update-all`，需要更新直接 `M-x package-update-all` 即可。

# 按键绑定

- [如何管理我的按键绑定](https://github.com/haoran-mc/.emacs.d/blob/main/docs/master-keybindings.md)
- [系统自带的按键与配置的按键](https://github.com/haoran-mc/.emacs.d/blob/main/docs/shortcuts.md)

# prog-mode

## golang-mode

使用 `lsp-bridge` 作为补全、符号查找的工具，默认后端使用 `gopls`，需要额外安装 `gopls` 的包。

# 个人文件快速打开

为一些常用的文件设置函数并绑定了快捷键，具体的位置在 [docs/secret_files.md](https://github.com/haoran-mc/.emacs.d/blob/main/docs/secret_files.md) 这个文件里说明了。

# Emacs 最小配置

当 Emacs 配置出问题时，可以使用 [init-mini.el](https://github.com/haoran-mc/.emacs.d/blob/main/init-mini.el) 这个最小配置来临时救急一下。

``` bash
emacs -Q -l init-mini.el
```
