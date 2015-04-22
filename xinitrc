xsetroot -cursor_name left_ptr
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xset r rate 200 30
setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift
setxkbmap -option ctrl:swapcaps
urxvtd -q -o -f
wmname LG3D
exec xmonad
