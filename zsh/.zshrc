#!/bin/sh
## StarShip Prompt
eval "$(starship init zsh)"
export STARSHIP_CONFIG=~/config/zsh/.zsh/prompt-starship.toml
export ZDOTDIR=$HOME/config/zsh/.zsh/
HISTFILE=~/config/zsh/.zsh/.zhistory
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

# Useful Functions
source "$ZDOTDIR/functions"

# Normal files to source
zsh_add_file "exports"
zsh_add_file "vim-mode"
zsh_add_file "aliases"
zsh_add_file "keybinds"
# Plugins
zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"
zsh_add_plugin "agkozak/zsh-z"
zsh_add_plugin "asdf-vm/asdf"


# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
# bindkey '^e' edit-command-line

# TODO Remove these
setxkbmap -option caps:escape
xset r rate 210 40

# Speedy keys
xset r rate 210 40

# Environment variables set everywhere
export EDITOR="lvim"
export TERMINAL="alacritty"
export BROWSER="firefox"

# For QT Themes
export QT_QPA_PLATFORMTHEME=qt5ct

# remap caps to escape
setxkbmap -option caps:escape
# swap escape and caps
# setxkbmap -option caps:swapescape




#neofetch
#screenfetch
#alsi
#paleofetch
colorscript random

# Zsh dinamyic directory in Dotfiles 
