#!/bin/env bash
# XMonad Rrecompile Error Logger
#----------------- 
red='\033[1;31m'
rset='\033[0m'
grn='\033[1;32m'
ylo='\033[1;33m'
blue='\033[1;34m'
#-----------------
echo -e "$grn  ----------------------------------------------------------------$rset"
echo -e "$blue                       XMonad Recompile :: $blue                 $rset"
echo -e "$grn  ----------------------------------------------------------------$rset$red\n"
xmonad --recompile && xmonad --restart && kill -9 $PPID || echo ""
#-----------------

