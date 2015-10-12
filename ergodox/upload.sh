#!/bin/sh
make && sudo teensy_loader_cli -v -mmcu=atmega32u4 -w ergodox_pjrc.hex 
