#!/bin/bash

function current_status {
    printf "\e[33m⭑\e[0m %s\n" "$1"
}

ln -s ~/.emacs.d/init-mini.el ~/.emacs

current_status "clone 3rd party code"
git clone --depth 1 git@github.com:nashamri/spacemacs-theme.git ~/Documents/emacs/local-packages/spacemacs-theme
git clone --depth 1 git@github.com:manateelazycat/lazycat-theme.git ~/Documents/emacs/local-packages/lazycat-theme


current_status "installation successful 🚀"
