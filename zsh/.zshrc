#!/bin/sh
#--------------------
export ZDOTDIR=$HOME/config/zsh/.zsh/
source "$ZDOTDIR/functions.sh"
add_file "exports"
add_file "vim-mode"
add_file "aliases"
add_file "keybinds"
add_plugin "zsh-users/zsh-autosuggestions"
add_plugin "zsh-users/zsh-syntax-highlighting"
add_plugin "hlissner/zsh-autopair"
add_plugin "agkozak/zsh-z"
add_plugin "asdf-vm/asdf"
eval "$(starship init zsh)"
#--------------------
setopt appendhistory
# some useful options (man zshoptions)
setopt autocd extendedglob nomatch menucomplete
setopt interactive_comments
stty stop undef		# Disable ctrl-s to freeze terminal.
zle_highlight=('paste:none')
# beeping is annoying
unsetopt BEEP
# completions
autoload -Uz compinit
zstyle ':completion:*' menu select
# zstyle ':completion::complete:lsof:*' menu yes select
zmodload zsh/complist
# compinit
_comp_options+=(globdots)		# Include hidden files.
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
# Colors
autoload -Uz colors && colors
# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
# bindkey '^e' edit-command-line
# TODO Remove these
setxkbmap -option caps:escape
xset r rate 210 40
# Speedy keys
xset r rate 210 40
# remap caps to escape
setxkbmap -option caps:escape
# swap escape and caps
# setxkbmap -option caps:swapescape
colorscript random

