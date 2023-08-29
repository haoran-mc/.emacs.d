# 如何安排我的 emacs 快捷键


1. C-x          系统快捷键，emacs 原生的快捷键，一般不修改以 C-x 为前缀的快捷键
2. C-c C-c      这是每个 major-mode 的按键
3. C-c ?        C-c 后跟一个字母是 emacs 为用户保留的按键绑定，用户应该尽量将按键绑定到这里
4. C-c C-?      显然 `C-c ?` 这种并不能足以用户使用，在保证不会影响已有快捷键的情况下使用 C-c C-?
5. <leader>     evil 的按键绑定
6. <leader>m    evil 为 local 的按键绑定
7. M-?          在不能使用 <leader> 且比较重要的地方使用 meta 键设置快捷键


- emacs 建议用户应尽量将按键绑定到 `C-c ?` 下
- rule1: **所有的快捷键都将出现在 evil 中**
- 部分在插入模式下使用的快捷键使用 `C-c ?` 形式
- <leader> 与 `C-c ?` 不必一样


> 在 rule1 的准则下，由于 evil-keys 大而全，有时反而效率下降
>
> 比如 <leader>tt 切换 tab
> t 键是 QWERTY 键盘布局中最难接近的两个键之一，而 tab-switch 是个常用的操作，所以在 C-c ? 中添加这个操作，C-c s (switch)
>
> 为什么不在 <leader> 中添加？
> 常用或不常用但偶尔使用会提高效率、幸福度的快捷键会放入 evil-keys，这导致 evil-keys 大而全，使用一个 <leader> ? 太奢侈
> evil-keys 首要考虑的是「正确地联想」，每个操作放入能够准确快速联想的字符前缀里。

详细的快捷键设置[在这里](./shortcuts.md)

# 以 C-c 为前缀的快捷键

- C-c a         org-agenda
- C-c b         lsp-bridge 返回
- C-c e +       eshell 的前缀
- C-c i +       插入文本（这个按键保留下来了，插入模式下只能使用 C-c ?）
- C-c j         lsp-bridge 前往函数定义处
- C-c r         lsp-bridge 前往函数使用处
- C-c p +       projectile
- C-c s         tab-switch
- C-c t +       hl-todo
- C-c x         org-capture

- C-c R         lsp-bridge 变量、函数改名
- C-c &         org-mode 链接跳转返回
- C-c /         webjump

- C-c u f       +unfill-paragraph

- C-c C-c       golang、elisp、org-src 运行代码
- C-c C-d       golang doc

# 以 +func 为首的自定义函数
