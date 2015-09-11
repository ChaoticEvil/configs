setxkbmap -layout "us,ru" -option "grp:caps_toggle,grp_led:scroll"

#while xsetroot -name "`date +"[%d.%m.%Y] %H:%M:%S"`"
#	do
#        	sleep 1
#   	 done &

exec feh --bg-scale data/media/wallpapers/more-volny-gorizont-1070.jpg &
#exec ~/opt/dwm/dwm

exec cwm -c ~/.cwmrc &&
exec xclock
#exec urxvt
#exec mate-session
