# 记录常用的快捷键
## 内置快捷键

- C-[  ESC
- C-m  RET
- C-i  TAB
- ctrl x ctrl f  查询、创建文件
- ctrl x 0       关闭当前窗口
- ctrl x o       光标多窗口切换
- ctrl c ctrl q  只读可写转换

## 自定义 C-x 为前缀的快捷键

- C-x g      margit-status
- C-x M-g    magit-dispatch

## 自定义 C-c 为前缀的快捷键

**global**
|------------------------|
| `C-c /` webjump        |
| `C-c e` +aweshell      |
| `C-c p` +projectile    |
| `C-c s` rg-menu        |
| `C-c t` +hl-todo       |
| `C-c y` yasnippet      |

**dired-mode**
- C-c C-e  wdired-change-to-wdired-mode

**minibuffer-local**
- C-c C-c  embark-export
- C-c C-o  embark-collect

**org-src-mode**
- C-c C-c  org-edit-src-exit

**isearch-mode**
- C-c C-o  isearch-occur

**lsp-bridge-mode**
- C-c b    +lsp-bridge-jump-back
- C-c j    +lsp-bridge-jump
- C-c r    lsp-bridge-rename
- C-c R    lsp-bridge-find-refenerce

**go-mode**
- C-c C-c  +go-run-buffer

**emacs-lisp-mode**
- C-c C-c  eval-to-comment

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
- ctrl c ctrl ,  插入模板

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
- ctrl c ctrl c m  查看转换成的html
- ctrl c ctrl c p  浏览器中预览markdown
- ctrl c ctrl c e  同目录下生成html
- ctrl c ctrl c v  同目录下生成html后在浏览器中预览
- ctrl c ctrl c l  直接在emacs中预览markdown

### yas-snippet

- [X] ctrl c y n  创建新的snippet
- [X] ctrl c y s  插入snippet
- [X] ctrl c y v  查看所有的snippet

### lsp-bridge

- [X] ctrl c j  进入函数
- [X] ctrl c b  返回进入位置
- [X] ctrl c r  变量重命名
- [X] ctrl c R  跳转到引用

### go-mode

- [X] ctrl c ctrl c  运行当前buffer
- ctrl c ctrl a      添加import
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
