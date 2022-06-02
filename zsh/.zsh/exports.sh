#!/bin/sh
HISTFILE=~/config/zsh/.zsh/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000
export PATH="$HOME/.local/bin":$PATH
export PATH="$PATH:$(yarn global bin)" 
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:./node_modules/.bin"
source $HOME/config/zsh/plugins/asdf/asdf.sh
source $HOME/.ghcup/env
export STARSHIP_CONFIG=~/config/zsh/.zsh/prompt-starship.toml
export EDITOR="lvim"
export TERMINAL="alacritty"
export BROWSER="firefox"
export QT_QPA_PLATFORMTHEME=qt5ct

# source ~/.local/share/fonts/i_linux.sh

