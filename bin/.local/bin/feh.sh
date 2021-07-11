#!/bin/bash


file=$(find ~/Pictures/.wallpaper/ -type f | shuf -n1) && DISPLAY=:0 feh --bg-scale $file
