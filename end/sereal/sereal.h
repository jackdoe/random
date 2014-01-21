#ifndef __SEREAL_H
#define __SEREAL_H
#include "obj.h"
#include "proto.h"

#define S_RECURSE_INC(s)                                            \
    do {                                                            \
        if((s)->level++ > MAX_RECURSION_DEPTH)                      \
            s_raise((s),rb_eArgError,                               \
                    "max recursion depth reached: %d (level: %d)",  \
                    MAX_RECURSION_DEPTH, s->level);                 \
    } while(0);

#define S_RECURSE_DEC(s) ((s)->level--)
#define TRUE 1
#define FALSE 0
#define MAX_RECURSION_DEPTH 100
#define COPY(src,dst,len) memcpy((dst),(src),len)
#define ZERO(src,len) memset((src),0,len)

#define is_ascii_string(str) (rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT)
#define THRESH(x,min,max) ((x) >= (min) && (x) <= (max))

#define IS_SHORT_BINARY(t) THRESH(t,SRL_HDR_SHORT_BINARY_LOW,SRL_HDR_SHORT_BINARY_HIGH)
#define IS_ARRAYREF(t) THRESH(t,SRL_HDR_ARRAYREF_LOW,SRL_HDR_ARRAYREF_HIGH)
#define IS_HASHREF(t) THRESH(t,SRL_HDR_HASHREF_LOW,SRL_HDR_HASHREF_HIGH)
#define IS_STRING(t) ((t) == SRL_HDR_STR_UTF8 || (t) == SRL_HDR_BINARY || IS_SHORT_BINARY((t)))

#define SRL_HDR_SYM SRL_HDR_RESERVED_LOW
#define SRL_HDR_RB_OBJ (SRL_HDR_RESERVED_LOW+1)

#define __RAW           0
#define __SNAPPY        1
#define __SNAPPY_INCR   2
#define __REF           4
#define __DEBUG         8
#define __NOT_MINE      16
#define __STREAM        32
#define __THAW          64
#define __COPY          64
#define __ARGUMENT_FLAGS (__DEBUG|__THAW|__REF|__COPY)

void s_init_writers(void);
obj decode(obj *payload);
obj encode(obj *payload);

#define __MIN_SIZE      6
#endif
