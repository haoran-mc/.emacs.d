#!/bin/bash

function current_status {
    printf "\e[33m⭑\e[0m %s\n" "$1"
}

ln -s ~/.emacs.d/init-mini.el ~/.emacs

current_status "installation successful 🚀"
