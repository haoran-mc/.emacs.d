#!/bin/bash

function __mkdir {
    local dir=$1

    if [ ! -d "${dir}" ]; then
        mkdir -p "${dir}"
    fi
}


__mkdir ~/haoran/tr/code-cpp
touch ~/haoran/tr/code-cpp/main.cpp
cp ~/.emacs.d/resources/compile_commands.json ~/haoran/tr/code-cpp/compile_commands.json

__mkdir ~/haoran/tr/code-go
touch ~/haoran/tr/code-go/main.go

__mkdir ~/haoran/tr/code-json
touch ~/haoran/tr/code-json/json.json
touch ~/haoran/tr/code-json/2.json

__mkdir ~/haoran/tr/code-py
touch ~/haoran/tr/code-py/main.py

__mkdir ~/haoran/tr/code-sql
touch ~/haoran/tr/code-sql/main.sql

__mkdir ~/haoran/tr/emacs-test
touch ~/haoran/tr/emacs-test/test.txt
touch ~/haoran/tr/emacs-test/huma.txt

__mkdir ~/haoran/tr/code-shell
touch ~/haoran/tr/code-shell/script.sh
chmod +x ~/haoran/tr/code-shell/script.sh
