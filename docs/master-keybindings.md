# 如何安排我的 emacs 快捷键


1. C-x          系统快捷键，emacs 原生的快捷键，一般不修改以 C-x 为前缀的快捷键
2. C-c C-c      这是每个 major-mode 的按键
3. C-c ?        C-c 后跟一个字母是 emacs 为用户保留的按键绑定，用户应该尽量将按键绑定到这里
4. C-c C-?      显然 `C-c ?` 这种并不能足以用户使用，在保证不会影响已有快捷键的情况下使用 C-c C-?
5. <leader>     evil 的按键绑定
6. <leader>m    evil 为 local 的按键绑定
7. M-?          在不能使用 <leader> 且比较重要的地方使用 meta 键设置快捷键


- 用户应尽量将按键绑定到 `C-c ?` 下
- 所有的快捷键都将出现在 evil 中，部分在插入模式下也可以使用的快捷键使用 `C-c ?` 形式
- <leader> 与 `C-c ?` 不必一样


详细的快捷键设置[在这里](./shortcuts.md)

# 以 C-c 为前缀的快捷键

- C-c a         org-agenda
- C-c b         lsp-bridge 返回
- C-c e +       eshell 的前缀
- C-c i         插入文本
- C-c j         lsp-bridge 前往函数定义处
- C-c r         lsp-bridge 前往函数使用处
- C-c p +       projectile 的前缀
- C-c t +       hl-todo 的前缀
- C-c x         org-capture
- C-c y +       yasnippet 的前缀

- C-c R         lsp-bridge 变量、函数改名
- C-c &         org-mode 链接跳转返回
- C-c /         webjump

- C-c u f       +unfill-paragraph)
- C-c C-c       golang、elisp、org-src 运行代码
- C-c C-d       golang doc
