#!/bin/sh

# Colorize Commands
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Programs 
alias 3D='/usr/bin/SplineDesign' # 3D Web Dev Program
alias torrent='transmission-gtk &!'
alias google='/usr/bin/google-chrome-stable'
alias x='et'
alias pdf='wpspdf'
alias l='lvim'
alias icue='ckb-next &!'

# Changing "ls" to "exa" 
alias ls='exa --color=always --icons --group-directories-first '
alias ll='exa -al --color=always --icons --group-directories-first' # my preferred listing 
alias lsa='exa -a --color=always --icons --group-directories-first '  # all files and dirs 
alias li='exa -l --color=always --icons --group-directories-first'  # long format 
alias l.='exa -a | egrep "^\."'

#  General
alias cp='cp -i'
alias mv='mv -i'
alias rmf='rm -i -rf'
alias mk='mkdir'
alias ctemp='rmf ~/.temp/ && mkdir ~/.temp' # Clear .temp file
alias b='yarn ' # use Yarn to utilize blitz
alias cpust='stress --cpu 16 --timeout 4' # Stress CPU To Test Temperatures
alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
#alias rm='trash-rm -i'
alias disk='ncdu' # File disk analyzer 
alias pkgs='pacman -Qqe | grep -v "$(pacman -Qqm)" > ~/.temp/pacman.lst && lvim ~/.temp/pacman.lst' # Generate Package list in $HOME/.temp/

# Zsh
alias zshrc="l $ZDOTDIR/.zshrc"
alias zc="l $ZDOTDIR"
alias zk="l $ZDOTDIR/keybinds" 
alias za="l $ZDOTDIR/aliases"
alias zp="l $ZDOTDIR/prompt-starship.toml"
alias zupdt="find "$ZDOTDIR/plugins" -type d -exec test -e '{}/.git' ';' -print0 | xargs -I {} -0 git -C {} pull -q"

# copy to dotfiles
alias xmd='l ~/.xmonad/xmonad.hs'
alias xmb='l ~/.xmobar.hs'

# others
alias c='clear' 
alias cpv='time rsync -ah --info=progress2'
alias pk='killall picom'
alias p='picom -b '
alias ph='picom -h | grep animation'
alias ph2='cat ~/.h/.ph'


