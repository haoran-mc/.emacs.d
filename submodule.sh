#!/bin/bash

set -e

function print_succ {
    printf "\e[33m⭑\e[0m success 🚀\n"
}

# https://stackoverflow.com/a/17692710/14093697
if [ $# -eq 0 ] || ([ $# -eq 1 ] && ([ $1 == '-h' ] || [ $1 == '--help' ])); then
    echo "usage: ./submodule.sh [-h | --help]"
    echo "                      [init]"
    echo "                      [add <repo-url>]"
    echo "                      [update <path-to-module>]"
    echo "                      [remove <path-to-module>]"
    echo "                      [branch <path-to-module>]"
    echo ""
    echo -e "\033[31msamples of using this script:\033[0m"
    echo ""
    echo -e "   \033[36minit\033[0m       \033[32m./submodule.sh init\033[0m"
    echo "              git submodule update --init --recursive"
    echo ""
    echo -e "   \033[36madd\033[0m        \033[32m./submodule.sh add\033[0m \033[34mgit@github.com:haoran-mc/.emacs.d.mini.git ./site-lisp/.emacs.d.mini\033[0m"
    echo "              git submodule add --depth 1 git@github.com:<repo-path> path/to/submodule"
    echo "              git config -f .gitmodules submodule.path/to/submodule.shallow true"
    echo ""
    echo -e "   \033[36mupdate\033[0m     \033[32m./submodule.sh update\033[0m \033[34msite-lisp/.emacs.d.mini\033[0m"
    echo "              git submodule update --remote ./site-lisp/.emacs.d.mini"
    echo ""
    echo -e "   \033[36mbranch\033[0m     \033[32m./submodule.sh branch\033[0m \033[34msite-lisp/.emacs.d.mini dev\033[0m"
    echo "              git config -f .gitmodules submodule.xxx.branch dev"
    echo ""
    echo -e "\033[31mcautions are these:\033[0m"
    echo ""
    echo "- no support update all, man cause crash;"
    echo "- remove operation is not currently supported;"
    echo ""
    exit
fi

if [ $# -eq 1 ] && [ $1 == "init" ]; then
    git submodule update --init --recursive
    print_succ
    exit
fi

if [ $# -eq 3 ] && [ $1 == "add" ]; then
    url=$2
    path_to_submodule=$3

    git submodule add --depth 1 ${url} ${path_to_submodule}

    # strip prefix: ./site-lisp -> site-lisp
    if [[ ${path_to_submodule} =~ ^\.\/* ]]; then
        path_to_submodule=${path_to_submodule:2}
    fi

    git config -f .gitmodules submodule."${path_to_submodule}".shallow true

    # https://stackoverflow.com/a/57551051/14093697
    git config -f .gitmodules submodule."${path_to_submodule}".ignore all
    print_succ
    exit
fi

if [ $# -eq 2 ] && [ $1 == "update" ]; then
    path_to_submodule=$2
    git submodule update --remote ${path_to_submodule}
    print_succ
    echo "then you may need commit this change."
    # add, commit and push
    exit
fi

if [ $# -eq 2 ] && [ $1 == "branch" ]; then
    path_to_submodule=$2

    # strip prefix: ./site-lisp -> site-lisp
    if [[ ${path_to_submodule} =~ ^\.\/* ]]; then
        path_to_submodule=${path_to_submodule:2}
    fi

    git config -f .gitmodules submodule."${path_to_submodule}".branch dev
fi

echo "parameter is not support!"
