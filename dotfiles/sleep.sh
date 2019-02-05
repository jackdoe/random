#!/bin/sh
case $(awk '{print $2}' /proc/acpi/button/lid/LID0/state) in
    closed) 
	    xset dpms force off
	    DISPLAY=:0 su -c - jack /usr/bin/slock &
            pm-suspend &
            ;;
    open)  xset dpms force on ;;
esac
