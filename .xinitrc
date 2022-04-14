#!/bin/sh

LANG='en_US.UTF-8'

if [ -f $HOME/.Xresources ]; then
  xrdb -merge $HOME/.Xresources

  if [ $(hostnamectl hostname) = 'mikepjb-laptop' ]; then
    xrdb -merge <<< 'Xft.dpi: 192'
  fi
fi

setxkbmap -layout us -option ctrl:nocaps
xset r rate 200 25

if [ -f $HOME/.Xmodmap ]; then
  xmodmap $HOME/.Xmodmap # is this needed anymore? probably for alt gr on laptop
fi

# TODO set the repeat/delay rate for keyboards

exec cinnamon-session
