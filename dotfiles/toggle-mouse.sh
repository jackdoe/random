xinput list-props 12 | grep "Device Enabled" | cut -f 2 -d ':' | grep 1
disabled=$?
echo $disabled
if [ $disabled = 1 ]; then
    xinput set-prop 12 "Device Enabled" 1
else
    xinput set-prop 12 "Device Enabled" 0
fi
