#!/bin/bash

function current_status {
    printf "\e[33m⭑\e[0m %s\n" "$1"
}

cp secret.el .secret.el

current_status "installation successful 🚀"
