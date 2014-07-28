xsetroot -cursor_name left_ptr
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
setxkbmap -layout us -variant dvp -option compose:102 -option numpad:shift3 -option kpdl:semi -option keypad:atm -option caps:shift
xset r rate 200 30
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "add mod4 = Caps_Lock"
xmodmap ~/.xmodmap
urxvtd -q -o -f
wmname LG3D
exec xmonad
