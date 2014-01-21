#ifndef __OBJ_H
#define __OBJ_H
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <locale.h>
#include <wchar.h>
#include <stdlib.h>
#include <time.h>
#define FOREACH_ARRAY(o,i) for ((i) = 0; (i) < ary_len(o); (i)++)
#define O_INT64_VALUE(o) (*((i64 *) ((o)->blob.data)))
#define FORMAT(fmt,arg...) fmt " %s()\n",##arg,__func__
#define D(fmt,arg...) printf(FORMAT(fmt,##arg))
#define SAYX(fmt,arg...)                        \
    do {                                        \
        D(fmt,##arg);                           \
        exit(EXIT_FAILURE);                     \
    } while(0);

typedef uint64_t    u64;
typedef uint32_t    u32;
typedef uint16_t    u16;
typedef uint8_t     u8;
typedef int64_t     i64;
struct blob {
    u8 *data;
    u32 size;
    u32 rsize;
};

struct object {
    u8 type;
    u8 flags;
    struct blob blob;
    union {
        struct {
            u32 pos;
            u32 len;
        } location;
        u64 u64data;
    } user;
};

#define T_OBJECT 0
#define T_ARRAY  1
#define T_HASH   T_ARRAY // fake hash (key value pairs represented in array)
#define T_NIL    2
#define T_TRUE   3
#define T_FALSE  4
#define T_INT64  7
#define T_DATA   8
#define T_MAX    9
#define HEADER 2
#define VALUE  4
#define BODY   8

typedef struct object obj;
typedef struct blob blob;

void obj_init_primitive(void);
void obj_destroy_primitive(void);
inline void must_be(obj *o, u8 type);
inline void *x_realloc(void *p, u32 n);
inline void *x_malloc(u32 n);
inline void b_append(struct blob *b, void *data, u32 size);

inline void obj_destroy(obj *o);
inline void obj_init(obj *o, u8 type);
inline obj obj_new(u8 type);
inline int obj_write(obj *, int);
obj obj_serialize(obj *o);
obj obj_deserialize(obj *o);
void obj_dump(obj *o);

inline u32 ary_len(obj *ary);
inline obj *ary_item(obj *ary, int index);
inline obj *ary_item_or_null(obj *ary, int index);
inline obj *ary_last(obj *ary);
inline void ary_push(obj *ary, obj *item);
inline void ary_push_s(obj *ary, obj item);
inline obj ary_new(void);
inline obj ary_pop(obj *ary);
inline int ary_is_valid_index(obj *ary, int index);
inline obj int64_new(int64_t value);

inline obj hash_new(void);
inline obj *hash_get(obj *h, obj *key);
inline obj* hash_get_raw(obj *h, u8 type, u8 *buf, u32 len);
inline void hash_set(obj *h, obj *key, obj *value);

inline obj data_new(void);
inline obj data_new2(u8 *buf, u32 len);
inline void data_concat(obj *, u8 *, u32);
inline obj data_extract(obj *s, u32 off, u32 len);
inline void data_split(obj *ary, obj *s, u32 off, u32 len, u8 splitter, u8 subsplitter);
inline u32 data_len(obj *s);
inline u8* data_ptr(obj *s,u32 off);

extern obj Qnil;
extern obj Qtrue;
extern obj Qfalse;
#endif
