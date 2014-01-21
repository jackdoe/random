#include "sereal.h"
static void (*WRITER[T_MAX])(obj *,obj *);
static void object_to_sereal(obj *s, obj *);
static void s_append_string(obj *s, obj *);
static void s_append_array(obj *s, obj *);
static void s_append_integer(obj *s, obj *);
static void s_append_true(obj *s, obj  *);
static void s_append_false(obj *s, obj  *);
static void s_append_nil(obj *s, obj  *);
static void s_default_writer(obj *s, obj *);
static inline void s_append_zigzag(obj *s,long long n);
static inline void s_append_u8(obj *s,u8 b);
static inline void s_append_u32(obj *s,u32 b);

static inline void s_append_u8(obj *s,u8 b) {
    data_concat(s,(u8 *) &b,(u32)sizeof(b));
}

static inline void s_append_u32(obj *s,u32 b) {
    data_concat(s,(u8 *) &b,(u32)sizeof(b));
}

void s_init_writers(void) {
    u32 i;
    for (i = 0; i < sizeof(WRITER)/sizeof(WRITER[0]); i++)
        WRITER[i] = s_default_writer;

    WRITER[T_INT64]  = s_append_integer;
    WRITER[T_DATA]   = s_append_string;
    WRITER[T_ARRAY]  = s_append_array;
    WRITER[T_TRUE]   = s_append_true;
    WRITER[T_FALSE]  = s_append_false;
    WRITER[T_NIL]    = s_append_nil;
}

static void s_default_writer(obj *s, obj *object) {
    SAYX("invalid type for input %d",object->type);
}

static inline void s_append_varint(obj *s,u64 n) {
    while (n >= 0x80) {
        s_append_u8(s,((n & 0x7f) | 0x80));
        n >>= 7; 
    }
    s_append_u8(s,n);
}

static inline void s_append_hdr_with_varint(obj *s,u8 hdr, u64 n) {
    s_append_u8(s,hdr);
    s_append_varint(s,n);
}

static inline void s_append_zigzag(obj *s,long long n) {
    s_append_hdr_with_varint(s,SRL_HDR_ZIGZAG,(n << 1) ^ (n >> 63));
}

static inline void s_append_string(obj *s,obj *object) {
    int len = data_len(object);
    if (len < SRL_MASK_SHORT_BINARY_LEN) {
        s_append_u8(s,SRL_HDR_SHORT_BINARY_LOW | (u8)len);
    } else {
        s_append_hdr_with_varint(s,SRL_HDR_BINARY,len); 
    }
    data_concat(s,data_ptr(object,0),data_len(object));
}

#define REF_THRESH(thresh,low,high)                     \
    do {                                                \
        if (len < (thresh))                             \
            s_append_u8(s, low | (u8) len);             \
        else                                            \
            s_append_hdr_with_varint(s,high,len);       \
    } while(0);

static void s_append_array(obj *s, obj *object) {
    u32 i,len = ary_len(object);
    REF_THRESH(SRL_MASK_ARRAYREF_COUNT,SRL_HDR_ARRAYREF,SRL_HDR_ARRAY);
    FOREACH_ARRAY(object,i) {
        object_to_sereal(s,ary_item(object,i));
    }
}

#undef REF_THRESH

static void s_append_integer(obj *s, obj *object) {
    i64 v = O_INT64_VALUE(object);
    if (v >= 0) {
        if (v < 16) 
            s_append_u8(s,SRL_HDR_POS_LOW | (u8) v);
        else
            s_append_hdr_with_varint(s,SRL_HDR_VARINT,v);
    } else {
        if (v > -17)
            s_append_u8(s,SRL_HDR_NEG_LOW | ((u8) v + 32));
        else
            s_append_zigzag(s,v);            
    }
}

static void s_append_true(obj *s, obj  *object) {
    s_append_u8(s,SRL_HDR_TRUE);
}

static void s_append_false(obj *s, obj  *object) {
    s_append_u8(s,SRL_HDR_FALSE);
}

static void s_append_nil(obj *s, obj  *object) {
    s_append_u8(s,SRL_HDR_UNDEF);
}

static void object_to_sereal(obj *s, obj *object) {
    (*WRITER[object->type])(s,object);
}

obj encode(obj *payload) {
    obj s = data_new();
    u8 version = SRL_PROTOCOL_VERSION;
    s_append_u32(&s,SRL_MAGIC_STRING_LILIPUTIAN);
    s_append_u8(&s,version);
    s_append_u8(&s,0x0);
    object_to_sereal(&s,payload);
    return s;
}
