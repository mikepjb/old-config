#!/bin/sh

if [ -f $HOME/.Xresources ]; then
  xrdb -merge $HOME/.Xresources

  if [ $(hostnamectl hostname) = 'mikepjb-laptop' ]; then
    xrdb -merge <<< 'Xft.dpi: 192'
  fi
fi

if [ -f $HOME/.Xmodmap ]; then
  xmodmap $HOME/.Xmodmap
fi

# TODO set the repeat/delay rate for keyboards

exec cinnamon-session
