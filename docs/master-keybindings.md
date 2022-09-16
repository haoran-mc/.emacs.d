# 如何安排我的 emacs 快捷键


1. C-x          系统快捷键，emacs 原生的快捷键，一般不修改以 C-x 为前缀的快捷键
2. C-c C-c      这是每个 major-mode 的按键
3. C-c ?        C-c 后跟一个字母是 emacs 为用户保留的按键绑定
4. C-c C-?      显然 `C-c ?` 这种并不能足以用户使用，在保证不会影响已有快捷键的情况下使用 C-c C-?
5. <leader>     evil 的按键绑定
6. <leader> m   evil 为 local 的按键绑定
7. M-?          在不能使用 <leader> 且比较重要的地方使用 meta 键设置快捷键


- 所有的快捷键都将出现在 evil 中，部分在插入模式下也可以使用的快捷键使用 `C-c ?` 形式


详细的快捷键设置[在这里](./shortcuts.md)
