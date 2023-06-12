Use the following ordering for `use-package` keywords:

- :init 
- :config
- :commands
- :bind
- :bind-keymap
- :map
- :mode
- :interpreter
- :defer
- :hook
- :magic
- :magic-fallback
- :custom
- :custom-face
- :demand
- :if
- :ensure
- :disable
- :when
- :preface
- :load-path
- :after
  - (:after (foo bar))
  - (:after (:all foo bar))
  - (:after (:any foo bar))
  - (:after (:all (:any foo bar) (:any baz quux)))
  - (:after (:any (:all foo bar) (:all baz quux)))
- :requires
- :defines
- :functions
- :no-require
- :load-path
- :catch
- :diminish
- :delight
- :ensure
- :pin


在Emacs的配置文件中，`use-package`宏是一种常用的包管理工具，用于安装、配置和延迟加载包。`use-package`提供了一种结构化和可读性强的方式来组织和管理包的配置。

`use-package`宏通常包含以下两个关键字块：`:config`和`:custom`。

1. `:config`块：用于在包加载时配置和设置相关的行为。在`:config`块中的代码会在包被加载时立即执行。这是您可以为包进行设置和自定义的常用地方。例如，您可以在这里更改包的键绑定、添加钩子函数等。`:config`块中的代码会在包被加载时顺序执行。

2. `:custom`块：用于自定义包的变量。`:custom`块中的代码用于设置和修改包的自定义变量。这些变量可以在Emacs启动时生效，或者通过调用特定函数来应用更改。通常，这些变量会在`:config`块中使用，以配置包的行为。

使用场景：
- 使用`use-package`的`:config`块：当您需要在包加载时执行一些配置和设置时，例如更改包的行为、添加钩子函数等。
- 使用`use-package`的`:custom`块：当您需要修改包的自定义变量时，或者需要在Emacs启动时设置这些变量。

总结起来，`use-package`的`:config`块用于在包加载时进行配置和设置，而`:custom`块用于自定义包的变量。这两个块可以根据需要在同一个`use-package`表达式中使用，以便统一管理包的安装、配置和自定义。
