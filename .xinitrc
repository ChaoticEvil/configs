xset -b

setxkbmap -layout "us,ru" -option "grp:caps_toggle,grp_led:scroll"

#while xsetroot -name "`date +"[%d.%m.%Y] %H:%M:%S"`"
#	do
#		sleep 1
#	done &

exec feh --bg-scale ~/data/media/wallpapers/smert-skelet-siniy-mrak.jpg &
exec ~/opt/dwm/dwm

