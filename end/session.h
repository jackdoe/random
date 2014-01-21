#ifndef __SESSION_H
#define __SESSION_H
#include "obj.h"
#define SID_LEN 64

struct session {
    union {
        u8 bytes[SID_LEN];
        u64 ident;
    } sid;
    obj data;
};
void generate_sid(struct session *s);

#endif
