#!/bin/bash

function current_status {
    printf "\e[33m⭑\e[0m %s\n" "$1"
}

brew install wakatime-cli

cp secret.el .secret.el
ln -s ~/.emacs.d/init-mini.el ~/.emacs

current_status "installation successful 🚀"
