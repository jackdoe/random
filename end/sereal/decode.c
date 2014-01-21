#include "sereal.h"
#include "decode.h"


static inline u32 s_shift_position_bang(obj *s, u32 len) {
    s->user.location.pos += len;
    return len;
}

static inline void *s_get_p_at_pos(obj *s, u32 pos,u32 req) {
    // returning s->data[pos], so we just make size count from 0
    if (pos + req >= data_len(s)) {
        SAYX("position is out of bounds (%d + %d >  %d)",pos,req,data_len(s));
    }
    return data_ptr(s,pos);
}

static inline void *s_get_p_at_pos_bang(obj *s, u32 pos,u32 req) {
    void *p = s_get_p_at_pos(s,pos,req);
    s_shift_position_bang(s,req);
    return p;
}

static inline void *s_get_p_req_inclusive(obj *s, int req) {
    return s_get_p_at_pos(s,s->user.location.pos,req > 0 ? req - 1 : 0);
}
static inline void *s_get_p_req(obj *s, int req) {
    return s_get_p_at_pos(s,s->user.location.pos,req);
}
static inline void *s_get_p(obj *s) {
    return s_get_p_at_pos(s,s->user.location.pos,0);
}

static inline u8 s_get_u8(obj *s) {
    return *((u8 *) s_get_p(s));
}

static inline u8 s_get_u8_bang(obj *s) {
    u8 r = s_get_u8(s);
    s_shift_position_bang(s,1);
    return r;
}

static inline u32 s_get_u32_bang(obj *s) {
    u32 *r = (u32 *) s_get_p_at_pos(s,s->user.location.pos,sizeof(u32) - 1); /* current position + 3 bytes */
    s_shift_position_bang(s,sizeof(*r));
    return *r;
}

static inline float s_get_float_bang(obj *s) {
    float *f = (float *) s_get_p_at_pos(s,s->user.location.pos,sizeof(*f) - 1);
    s_shift_position_bang(s,sizeof(*f));
    return *f;
}

static inline double s_get_double_bang(obj *s) {
    double *d = (double *) s_get_p_at_pos(s,s->user.location.pos,sizeof(*d) - 1);
    s_shift_position_bang(s,sizeof(*d));
    return *d;
}

static inline long double s_get_long_double_bang(obj *s) {
    long double *d = (long double *) s_get_p_at_pos(s,s->user.location.pos,sizeof(*d) - 1);
    s_shift_position_bang(s,sizeof(*d));
    return *d;
}

static obj s_default_reader(obj *s, u8 tag) {
    SAYX("unsupported tag %d [ 0x%x ]",tag,tag);
    return Qnil;
}

/* VARINT */
static u64 s_get_varint_bang(obj *s) {
    u64 uv = 0;
    unsigned int lshift = 0;
    while (s_get_u8(s) & 0x80) {
        uv |= ((u64)(s_get_u8_bang(s) & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(uv) * 8))
            SAYX("varint too big");
    }
    uv |= ((u64)(s_get_u8_bang(s)) << lshift);
    return uv;
}

/* ZIGZAG */
static obj s_read_zigzag(obj *s, u8 tag) {
    i64 z = 0;
    u64 v = s_get_varint_bang(s);
    z = (v >> 1) ^ -(v & 0x01);
    return int64_new(z);
}

/* VARINT */
static obj s_read_varint(obj *s, u8 tag) {
    return int64_new(s_get_varint_bang(s));
}

/* POS */
static obj s_read_small_positive_int(obj *s, u8 tag) {
    return int64_new(tag);
}

/* NEG */
static obj s_read_small_negative_int(obj *s, u8 tag) {
    return int64_new(tag - 32);
}

/* ARRAY */
static inline obj s_read_array_with_len(obj *s, u32 len) {
    register u32 i;
    obj a = ary_new();
    for (i = 0; i < len; i++)
        ary_push_s(&a,sereal_to_obj(s));
    return a;
}

/* ARRAY */ 
static obj s_read_array(obj *s,u8 tag) {
    return s_read_array_with_len(s,s_get_varint_bang(s));
}

/* ARRAY */ 
static obj s_read_arrayref(obj *s, u8 tag) {
    return s_read_array_with_len(s,tag & SRL_MASK_ARRAYREF_COUNT);       
}

/* HASH */ 
static inline obj s_read_hash_with_len(obj *s, u32 len) {
    register u32 i;
    obj a = ary_new();
    for (i = 0; i < len; i++) {
        ary_push_s(&a,sereal_to_obj(s));
        ary_push_s(&a,sereal_to_obj(s));
    }
    return a;
}

/* HASH */ 
static obj s_read_hash(obj *s, u8 tag) {
    return s_read_hash_with_len(s,s_get_varint_bang(s));
}

/* HASH */ 
static obj s_read_hashref(obj *s, u8 tag) {
    return s_read_hash_with_len(s,tag  & SRL_MASK_HASHREF_COUNT);
}

static obj s_read_string(obj *s,u8 t) {
    u32 len = 0;
    obj string;
// len - 1: we also use the current byte, similar to u32,float.. casts
#define RETURN_STRING(fx_l,fx_gen)                             \
    do {                                                       \
        len = fx_l;                                            \
        u8 *ptr = len == 0 ? 0 : s_get_p_req_inclusive(s,len); \
        string = fx_gen;                                       \
        s_shift_position_bang(s,len);                          \
        return string;                                         \
    } while(0);

    if (t == SRL_HDR_BINARY) {
        RETURN_STRING(s_get_varint_bang(s),data_new2(ptr,len));
    } else if (IS_SHORT_BINARY(t)) {
        RETURN_STRING((t & SRL_MASK_SHORT_BINARY_LEN),data_new2(ptr, len));
    }
#undef RETURN_STRING
    SAYX("undefined string type %d",t);
}

static obj s_read_nil(obj *s, u8 tag) {
    return Qnil;
}

static obj s_read_true(obj *s, u8 tag) {
    return Qtrue;
}

static obj s_read_false(obj *s, u8 tag) {
    return Qfalse;
}

static obj s_read_pad(obj *s, u8 tag) {
    /* just skip this byte and go forward */
    return sereal_to_obj(s);
}

static obj s_read_extend(obj *s, u8 tag) {
    SAYX("extend tags are not supported");
}

obj sereal_to_obj(obj *s) {
    u8 t;
    while (s->user.location.pos < data_len(s)) {
        t = s_get_u8_bang(s);
        if (t & SRL_HDR_TRACK_FLAG)
            SAYX("tracking is not supported");
        return (*READERS[t])(s,t);
    }
    SAYX("bad packet, or broken decoder");
    return Qnil;
}

obj decode(obj *payload) {
    u32 magic = s_get_u32_bang(payload);
    if (magic != SRL_MAGIC_STRING_LILIPUTIAN)
        SAYX("invalid header: %d (%x)",magic,magic);

    u8 version = s_get_u8_bang(payload);
    u8 suffix = s_get_varint_bang(payload);

    if (version & SRL_PROTOCOL_ENCODING_MASK)
        SAYX("compression is not supported");

    return sereal_to_obj(payload);
}
