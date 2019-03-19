copied from:

https://www.reddit.com/r/MechanicalKeyboards/comments/5go5fj/photos_weekend_project_quick_commodore_64_usb/


to build:
```

git clone https://github.com/qmk/qmk_firmware
git clone https://github.com/jackdoe/random
mv random/c64 qmk_firmware/keyboards
cd qmk_firmware && make c64:default
```

to upload:
```
sudo teensy_loader_cli -v -mmcu=atmega32u4 -w c64_default.hex
```

edit keymap.c because i have placed capslock at the pace of control, because by default i have capslock -> ctrl xmodmapped