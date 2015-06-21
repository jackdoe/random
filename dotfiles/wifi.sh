#!/bin/sh

INTERFACE="iwm0"
DIR=/home/jack/.wifi/
PROFILES=$DIR/profiles
CONTROL="/tmp/force_wifi_reconnect"
WPA_SUPPLICANT_CONF=$DIR/wpa_supplicant.conf

RED='\033[0;31m'
BLUE='\033[0;34m'
ORANGE='\033[0;33m'
NC='\033[0m'

reconnect() {
    if [ ! -f $WPA_SUPPLICANT_CONF ]; then
        echo "missing $WPA_SUPPLICANT_CONF"
        exit 1
    fi

    pkill -9 wpa_supplicant > /dev/null 2>&1
    pkill -9 dhclient > /dev/null 2>&1
    /usr/local/sbin/wpa_supplicant -c $WPA_SUPPLICANT_CONF -D openbsd -i $INTERFACE -B || (echo "unable to start wpa_supplicant" && exit 1)

    ifconfig iwm0 -nwid -wpa -wpakey >/dev/null 2>&1; ifconfig iwm0 delete >/dev/null 2>&1; ifconfig iwm0 down >/dev/null 2>&1
    sleep 1

    ifconfig $INTERFACE scan > /tmp/scan.txt
    for profile in `ls -1 $PROFILES/`; do
        raw=`echo $profile | sed -e 's/_/ /g'`
        ssid=`echo $raw | cut -f 1 -d ' '`

        echo "scanning $INTERFACE for ${ORANGE}$ssid${NC}"
        cat /tmp/scan.txt | grep $ssid >/dev/null 2>&1

        if [ $? -eq 0 ]; then
            cmd="ifconfig $INTERFACE nwid $raw wpaakms psk,802.1x up"
            echo "${RED}found $ssid, executing $cmd${NC}"
            $cmd && dhclient $INTERFACE
            return 0
        fi

    done

    return 1
}

mkdir -p $PROFILES

chown -R root:wheel $DIR
chmod 0700 $DIR
chmod 0700 $PROFILES
chmod 0600 $WPA_SUPPLICANT_CONF

reconnect

sleeping=0
while :; do
    if [ -f $CONTROL ]; then
        reconnect
        rm -f $CONTROL
    fi
    if [ $sleeping -eq 10 ]; then
        echo "`date` ${BLUE}$0 sleeping${NC}"
        sleeping=0
    fi
    
    sleeping=$((sleeping+1))
    sleep 1;
done

# mkdir -p /home/jack/.wifi/profiles
# touch /home/jack/.wifi/profiles/some-ssid_wpa_wpakey_some-key
# cat > /home/jack/.wifi/wpa_supplicant.conf2 <<EOF
# ctrl_interface=/var/run/wpa_supplicant
# ctrl_interface_group=wheel
# ap_scan=0
# network={
#         ssid="some-other-ssid"
#         key_mgmt=WPA-EAP
#         eap=TTLS PEAP
#         identity="user"
#         password="password"
# }
# network={
#     ssid="some-ssid"
#     psk="some password"
#     key_mgmt=WPA-PSK
# }
# EOF
