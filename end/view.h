#ifndef __VIEW_H
#define __VIEW_H
#include "salt.h"
#define MAX_ARG 256
inline void h_open(obj *o,char *tag);
inline void h_close(obj *o,char *tag);
inline void text(obj *o,char *t);
void play(struct client *c);
#endif
