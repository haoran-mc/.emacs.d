# 记录常用的快捷键
## 计算机快捷键

- C-[  ESC
- C-m  RET
- C-i  TAB

## 原生快捷键（C-x）

- ctrl x ctrl f  查询、创建文件
- ctrl x 0       关闭当前窗口
- ctrl x o       光标多窗口切换

| `DEL` → backward-kill-sentence            | `4` → `+ctl-x-4-prefix`         | `g` → magit-status                | `C-SPC` → pop-global-mark          | `C-q` → read-only-mode          |
| `ESC` → repeat-complex-command            | `5` → `+ctl-x-5-prefix`         | `h` → mark-whole-buffer           | `C-+` → text-scale-adjust          | `C-r` → find-file-read-only     |
| `RET` → `+prefix`                         | `6` → `+2C-command`             | `i` → insert-file                 | `C--` → text-scale-adjust          | `C-s` → save-buffer             |
| `SPC` → rectangle-mark-mode               | `8` → `+prefix`                 | `k` → kill-buffer                 | `C-0` → text-scale-adjust          | `C-t` → transpose-lines         |
| `TAB` → indent-rigidly                    | `;` → comment-set-column        | `l` → count-lines-page            | `C-;` → comment-line               | `C-u` → upcase-region           |
| `$` → set-selective-display               | `<` → scroll-left               | `m` → compose-mail                | `C-=` → text-scale-adjust          | `C-v` → find-alternate-file     |
| `'` → expand-abbrev                       | `=` → what-cursor-position      | `n` → `+narrow`                   | `C-@` → pop-global-mark            | `C-w` → write-file              |
| `(` → kmacro-start-macro                  | `>` → scroll-right              | `o` → other-window                | `C-b` → list-buffers               | `C-x` → exchange-point-and-mark |
| `)` → kmacro-end-macro                    | `[` → backward-page             | `q` → kbd-macro-query             | `C-c` → save-buffers-kill-terminal | `C-z` → suspend-frame           |
| `*` → calc-dispatch                       | `]` → forward-page              | `r` → `+prefix`                   | `C-d` → list-directory             | `M-:` → repeat-complex-command  |
| `+` → balance-windows                     | `^` → enlarge-window            | `s` → save-some-buffers           | `C-e` → eval-last-sexp             | `M-g` → magit-dispatch          |
| `-` → shrink-window-if-larger-than-buffer | \` → next-error                 | `t` → `+tab`                      | `C-f` → find-file                  | `C-left` → previous-buffer      |
| `.` → set-fill-prefix                     | `a` → `+abbrev`                 | `u` → undo                        | `C-k` → `+kmacro-keymap`           | `C-right` → next-buffer         |
| `0` → delete-window                       | `b` → switch-to-buffer          | `v` → `+vc-prefix-map`            | `C-l` → downcase-region            | `←` → previous-buffer          |
| `1` → delete-other-windows                | `d` → dired                     | `z` → repeat                      | `C-n` → set-goal-column            | `→` → next-buffer              |
| `2` → split-window-below                  | `e` → kmacro-end-and-call-macro | `{` → shrink-window-horizontally  | `C-o` → delete-blank-lines         |                                  |
| `3` → split-window-right                  | `f` → set-fill-column           | `}` → enlarge-window-horizontally | `C-p` → mark-page                  |                                  |

未被系统使用的以 C-x 为前缀的快捷键：

| C-x ~ | C-x \  | C-x j    | C-x C-# | C-x C-_ | C-x C-/   | C-x C-ESC |
| C-x ! | C-x ,  | C-x p    | C-x C-$ | C-x C-: | C-x C-?   | C-x C-a   |
| C-x @ | C-x /  | C-x w    | C-x C-% | C-x C-' | C-x C-\   | C-x C-h   |
| C-x # | C-x ?  | C-x x    | C-x C-^ | C-x C-" | C-x C-\|  | C-x C-j   |
| C-x % | C-x \| | C-x y    | C-x C-& | C-x C-< | C-x C-TAB | C-x C-y   |
| C-x & | C-x '  | C-x C-\` | C-x C-* | C-x C-> | C-x C-SPC |           |
| C-x _ | C-x "  | C-x C-~  | C-x C-( | C-x C-, | C-x C-DEL |           |
| C-x 9 | C-x c  | C-x C-!  | C-x C-) | C-x C-. | C-x C-RET |           |

## 自定义 C-x 为前缀的快捷键

- C-x 1      treemacs-delete-other-windows
- C-x t 1    treemacs-delete-other-windows
- C-x t t    treemacs
- C-x t b    treemacs-bookmark
- C-x t C-t  treemacs-find-file
- C-x t M-t  treemacs-find-tag
- C-x g      margit-status
- C-x M-g    magit-dispatch

## 原生快捷键（C-c）

- ctrl c ctrl q  只读可写转换

## 自定义 C-c 为前缀的快捷键

**global**
| `C-c !` flycheck       |
| `C-c @` hideshow       |
| `C-c /` webjump        |
| `C-c B` devdocs-lookup |
| `C-c c` +citre         |
| `C-c e` +aweshell      |
| `C-c p` +projectile    |
| `C-c s` rg-menu        |
| `C-c t` +hl-todo       |
| `C-c y` yasnippet      |
| `C-c x` quickrun       |

**dired-mode**
- C-c C-e  wdired-change-to-wdired-mode

**minibuffer-local**
- C-c C-c  embark-export
- C-c C-o  embark-collect

**org-src-mode**
- C-c C-c  org-edit-src-exit

**markdown-mode**
- C-c m r  +markdown-insert-ruby-tag
- C-c m d  +markdown-insert-details
- C-c m g  grip-mode

**isearch-mode**
- C-c C-o  isearch-occur

**prog-mode**
- C-c '  separedit

**go-mode**
- C-c b    +lsp-bridge-jump-back
- C-c j    +lsp-bridge-jump
- C-c r    lsp-bridge-rename
- C-c t    hl-todo/tag/test
- C-c C-c  +go-run-buffer
- C-c C-u  go-remove-unused-imports

**emacs-lisp-mode**
- C-c b    +lsp-bridge-jump-back
- C-c j    +lsp-bridge-jump
- C-c r    lsp-bridge-rename
- C-c C-c  eval-to-comment

## 其他原生快捷键（ctrl）

**帮助**
- ctrl h v  查看变量
- ctrl h k  查看按键绑定
- ctrl h m  查看major-mode
- ctrl h f  查看函数
- ctrl h w  查看函数绑定按键
- ctrl h e  查看message buffer
- ctrl h o  查看标志
- ctrl h d  查看文档
- ctrl h c  简短介绍按键功能
- ctrl h b  查看键绑定
- ctrl h i  教程

**evil-normal-state**
- ctrl a  前往行首
- ctrl b  上翻一页
- ctrl c  +prefix
- ctrl d  下翻半页
- ctrl e  上移一行
- ctrl f  下翻一页
- ctrl g  keyboard-quit
- ctrl h  帮助
- ctrl i  evil-jump-forward
- ctrl j  electric-newline-and-maybe-indent
- ctrl k  删除光标到行尾
- ctrl l  recenter-top-bottom
- ctrl m  回车，部分 mode 有其他作用
- ctrl n  evil-paste-pop-next
- ctrl o  evil-jump-backward
- ctrl p  evil-paste-pop
- ctrl q  quoted-insert
- ctrl r  redo
- ctrl s  isearch
- ctrl t  pop-tag-mark
- ctrl u  上翻半页
- ctrl v  可视块
- ctrl w  +windows
- ctrl x  +prefix
- ctrl y  evil-scroll-line-up
- ctrl z  evil-exit-emacs-state

**evil-insert-state**
- ctrl a  前往行首
- ctrl b  后退一个字符
- ctrl c  +prefix
- ctrl d  删除前面一个字符
- ctrl e  前往行尾
- ctrl f  前进一个字符
- ctrl g  keyboark-quit
- ctrl h  帮助
- ctrl i  TAB
- ctrl j  electric-newline-and-maybe-indent
- ctrl k  删除光标到行尾
- ctrl l  recenter-top-bottom
- ctrl m  RET
- ctrl n  下一行
- ctrl o  下面创建新行
- ctrl p  上一行
- ctrl q  quoted-insert
- ctrl r  isearch-backward
- ctrl s  isearch
- ctrl t  交换当前字符与前一个字符
- ctrl u  重复插入
- ctrl v  scroll-up-command
- ctrl w  删除前一个单词
- ctrl x  +prefix
- ctrl y  粘贴
- ctrl z  evil-exit-emacs-state

## Meta 快捷键

- M-;  comment-or-uncomment
- M-:  eval-expression -> execute-extended-commend
- M-x  execute-extended-command

## 根据插件/功能分类，方便查找
### org-mode

- ctrl c ctrl l  创建、修改链接
- ctrl c ctrl o  打开链接（alias <RET>）
- ctrl c &       返回进入链接的位置
- ctrl c %       存储文件位置
- ctrl c ;       注释段落

### markdown-mode

- ctrl c ctrl s -  插入分割线
- ctrl c ctrl s f  插入脚注
- ctrl c ctrl s w  插入wikilink
- ctrl c ctrl n    下一个标题
- ctrl c ctrl p    上一个标题
- ctrl c ctrl f    下一个同级标题
- ctrl c ctrl b    下一个同级标题
- ctrl c ctrl u    前往所处标题的父标题
- ctrl c ctrl d    插入todo列表
- ctrl c ctrl o    在浏览器下打开链接
- [X] ctrl c m r   插入ruby-tag
- [X] ctrl c m d   插入详细样式
- [X] ctrl c m p   预览markdown
- ctrl c ctrl c m  查看转换成的html
- ctrl c ctrl c p  浏览器中预览markdown
- ctrl c ctrl c e  同目录下生成html
- ctrl c ctrl c v  同目录下生成html后在浏览器中预览
- ctrl c ctrl c l  直接在emacs中预览markdown

### treemacs

- [X] ctrl x t t       开启treemacs
- [X] ctrl x 1         删除其他窗口
- [X] ctrl x t 1       删除其他窗口
- [X] ctrl x t b       使用treemacs打开书签
- [X] ctrl x t ctrl t  使用treemacs寻找、打开文件
- [X] ctrl x t alt t   使用treemacs寻找、打开标签

### yas-snippet

- [X] ctrl c y n  创建新的snippet
- [X] ctrl c y s  插入snippet
- [X] ctrl c y v  查看所有的snippet

### lsp-bridge

- [X] ctrl c j  进入函数
- [X] ctrl c b  返回进入位置
- [X] ctrl c r  变量重命名

### go-mode

- [X] ctrl c ctrl c  运行当前buffer
- ctrl c ctrl a      添加import
- [X] ctrl c ctrl u  删除未使用的import
- ctrl c ctrl d      描述鼠标位置表达式
- ctrl c ctrl j      跳转到当前变量、函数定义
- ctrl x 4 ctrl c ctrl j  用其他窗口跳转当前变量、函数定义
- ctrl c ctrl f i  跳转到import
- ctrl c ctrl f a  跳转到参数列表
- ctrl c ctrl f d  跳转到函数描述
- ctrl c ctrl f f  跳转到函数关键字
- ctrl c ctrl f n  跳转到函数名
- ctrl c ctrl f r  跳转到返回值
- ctrl c ctrl f m  跳转到接收者
- ctrl-M-a  光标移动到函数定义开头
- ctrl-M-e  光标移动到函数定义末尾
- ctrl-M-h  标记当前函数的整个内容
- ctrl c t a  添加tag
- ctrl c t r  移除tag
- ctrl c t g  生成测试代码
- ctrl c t f  测试当前文件
- ctrl c t t  测试当前测试
- ctrl c t j  测试整个项目的测试代码
- ctrl c t b  基准测试
- ctrl c t c  平均测试
- ctrl c t x  运行当前代码
