xsetroot -cursor_name left_ptr
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift
xset r rate 200 30
setxkbmap -option ctrl:swapcaps
setxkbmap -option ctrl:nocaps
urxvtd -q -o -f
wmname LG3D
exec xmonad
