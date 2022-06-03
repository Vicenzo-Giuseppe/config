#!/bin/env bash
# Update from local files to Config folder
#----------------- 
red='\033[1;31m'
rset='\033[0m'
grn='\033[1;32m'
ylo='\033[1;33m'
blue='\033[1;34m'
#-----------------
echo -e "$grn  ----------------------------------------------------------------$rset"
echo -e "$blue                       Updating Config :: $blue                  $rset"
echo -e "$grn  ----------------------------------------------------------------$rset$red\n"
#-----------------

cp ~/.sh/.xmobar.hs ~/config/xmobar/.xmobar.hs 
cp ~/.config/alacritty/alacritty.yml ~/config/alacritty/alacritty.yml
cp ~/.sh/.autostart.sh ~/config/autostart/.autostart.sh 
cp ~/.zshrc ~/config/zsh/.zshrc 
cp ~/.sh/.picom.conf ~/config/picom/picom.conf
cp ~/.xmonad/xmonad.hs ~/config/xmonad/xmonad.hs
cp -r ~/.sh/* ~/config/.sh/
