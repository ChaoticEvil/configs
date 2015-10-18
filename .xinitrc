setxkbmap -layout "us,ru" -option "grp:caps_toggle,grp_led:scroll"

exec feh --bg-scale data/media/wallpapers/1.jpg &

#while xsetroot -name "`date +"[%d.%m.%Y] %H:%M:%S"`"
#	do
#        	sleep 1
#   	 done &
#exec ~/opt/dwm/dwm

exec cwm -c ~/.cwmrc
