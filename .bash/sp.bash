#!/bin/bash
# vi: filetype=sh
# -*- mode: sh -*-

function sp() {
    if [[ $1 == "" ]]; then
        cd $(projector list | fzf)
    else
        cd $(projector find $1)
    fi

    if [[ -d $(pwd)/.git ]]; then
        NAME=$(basename $(git rev-parse --show-toplevel))
        if [[ -d ~/.virtualenvs/$NAME ]]; then
            workon $NAME
        fi
    fi
}
