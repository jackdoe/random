ARCH=$(shell uname -i)
CFLAGS=-Ihttp-parser/ -D_GNU_SOURCE -D_BSD_SOURCE
all:
	perl -MExtUtils::Embed -e xsinit -- -o perlxsi.c
	gcc -g3 -c perlxsi.c `perl -MExtUtils::Embed -e ccopts`
	gcc -g3 -c salt.c $(CFLAGS)  `perl -MExtUtils::Embed -e ccopts`
	gcc -g3 -o SALT perlxsi.o salt.o `perl -MExtUtils::Embed -e ldopts` -lm -lrt -std=c99 -lpthread -lssl -lcrypto
