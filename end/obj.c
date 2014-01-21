#include "obj.h"
#include <inttypes.h>
#include "sereal/sereal.h"

obj Qnil;
obj Qtrue;
obj Qfalse;

inline void must_be(obj *o, u8 type) {
    if (!o || o->type != type)
        SAYX("expecting %x",type);
}

inline void *x_realloc(void *p, u32 n) {
    u8 *buf = realloc(p,n);
    if (!buf)
        SAYX("failed to allocate %d",n);
    return buf;
}

inline void *x_malloc(u32 n) {
    return x_realloc(NULL,n);
}

inline void b_append(struct blob *b, void *data, u32 size) {
    if ((b->size + size > b->rsize) || size == 0) {
        b->data = x_realloc(b->data,b->size + size + 128);
        b->rsize = b->size + size + 128;
    }
    if (size > 0) {
//        D("appending: %d to %d %p",size,b->size,b->data + b->size);
        memcpy(b->data + b->size, data, size);
        b->size += size;
    }
}

inline u32 ary_len(obj *ary) {
    must_be(ary,T_ARRAY);
    return ary->user.location.len;
}
inline int ary_is_valid_index(obj *ary, int index) {
   if (index < 0 || index >= ary_len(ary))
       return 0;
   return 1;
}

inline obj *ary_item_or_null(obj *ary, int index) {
    if (!ary_is_valid_index(ary,index))
        return NULL;
    return ary_item(ary,index);
}

inline obj *ary_item(obj *ary, int index) {
    if (!ary_is_valid_index(ary,index))
        SAYX("index out of bounds: %d (len: %d)",index,ary_len(ary));
    return (obj *) (ary->blob.data + (index * sizeof(obj)));
}

inline obj *ary_last(obj *ary) {
    if (ary_len(ary) == 0)
        return NULL;
    return ary_item(ary,ary_len(ary) - 1);
}

inline void ary_push_s(obj *ary, obj item) {
    ary_push(ary,&item);
}

inline void ary_push(obj *ary, obj *item) {
    must_be(ary,T_ARRAY);
    b_append(&ary->blob, item, sizeof(*item));
    ary->user.location.len++;
}

inline obj ary_pop(obj *ary) {
    obj *v = ary_item(ary,ary_len(ary) - 1);
    ary->user.location.len--;
    return *v;
}

inline void obj_destroy(obj *o) {
    if (o->type == T_ARRAY) {
        int i;
        FOREACH_ARRAY(o,i) {
            obj_destroy(ary_item(o,i));
        }
    }
    if (o->blob.data) {
        free(o->blob.data);
    } else {
        D("object without allocated data! (type: %d)",o->type);
    }
}

inline void obj_init(obj *o, u8 type) {
    memset(o,0,sizeof(*o));
    o->type = type;
    b_append(&o->blob,NULL,0);
}

inline obj obj_new(u8 type) {
    obj o;
    obj_init(&o,type);
    return o;
}

inline obj int64_new(int64_t value) {
    obj o = obj_new(T_INT64);
    b_append(&o.blob,&value, sizeof(value));
    return o;
}

inline obj data_new(void) {
    return obj_new(T_DATA);
}

inline obj data_new2(u8 *ptr, u32 len) {
    obj v = data_new();
    data_concat(&v,ptr,len);
    return v;
}

inline obj data_extract(obj *s, u32 off, u32 len) {
    obj cut = data_new();
    data_concat(&cut,data_ptr(s,off), len);
    return cut;
}

inline void data_split(obj *ary, obj *s, u32 off, u32 len, u8 splitter, u8 subsplitter) {
    int i;
    int end = off + len;
    for (i = off; i <= end; i++) {
        if (i == end || s->blob.data[i] == splitter) {
            if (i - off > 0) {
                obj extract = data_extract(s,off,i - off);
                if (subsplitter) {
                    data_split(ary,&extract,0,i - off, subsplitter, 0);
                    obj_destroy(&extract);
                } else {
                    ary_push(ary,&extract);
                }
            }
            off = i + 1;
        }
    }
}

inline u32 data_len(obj *s) {
    return s->user.location.len;
}

inline u8 *data_ptr(obj *s,u32 off) {
    if (off >= data_len(s))
        SAYX("request out of bounds %d offset %d size",off,data_len(s));
    return s->blob.data + off;
}

inline void data_concat(obj *s, u8 *buf, u32 len) {
    must_be(s,T_DATA);
    b_append(&s->blob,buf,len);
    s->user.location.len += len;
}

inline obj ary_new(void) {
    return obj_new(T_ARRAY);
}

void __obj_dump(obj *o, int level) {
    int i;
    for (i = 0; i < level; i++) {
        printf("  ");
    }
    if (!o) {
        printf("NULL-OBJECT\n");
        return;
    }

    switch(o->type) {
    case T_ARRAY:
        printf("ARRAY (len: %d)\n",ary_len(o));
        FOREACH_ARRAY(o,i) {
            __obj_dump(ary_item(o,i),level + 1);
        }
        break;
    case T_NIL:
        printf("NIL\n");
        break;
    case T_TRUE:
        printf("TRUE\n");
        break;
    case T_FALSE:
        printf("FALSE\n");
        break;
    case T_DATA:
        printf("DATA(len: %d):%.*s\n",data_len(o),data_len(o),data_ptr(o,0));
        printf("\t");
        for (i =0; i < data_len(o); i++)
            printf("%02x",*((char *) data_ptr(o,i)));
        printf("\n");
        break;
    case T_INT64:
        printf("INT64\t%" PRId64 "\n",O_INT64_VALUE(o));
        break;
    default:
        SAYX("unknown type: %d",o->type);
    }
}

void obj_dump(obj *o) {
    __obj_dump(o,0);
}

inline obj hash_new(void) {
    return ary_new();
}

inline obj* hash_get_raw(obj *h, u8 type, u8 *buf, u32 len) {
    int i;
    FOREACH_ARRAY(h,i) {
        obj *k = ary_item(h,i);
        if (k->type == type
            && k->blob.size == len
            && memcmp(k->blob.data,buf,len) == 0) {

            return ary_item_or_null(h,i+1);
        }
        i++;
    }
    return NULL;
}

// fake hash
obj* hash_get(obj *h, obj *key) {
    return hash_get_raw(h,key->type,key->blob.data,key->blob.size);
}

// fake hash
inline void hash_set(obj *h, obj *key, obj *value) {
    if (hash_get(h,key) != NULL) {
        ary_push(h,key);
        ary_push(h,value);
    }
}

inline void obj_init_primitive(void) {
    obj_init(&Qnil,T_NIL);
    obj_init(&Qtrue,T_TRUE);
    obj_init(&Qfalse,T_FALSE);
}

inline void obj_destroy_primitive(void) {
    obj_destroy(&Qnil);
    obj_destroy(&Qtrue);
    obj_destroy(&Qfalse);
}

inline int obj_write(obj *o, int fd) {
    int rc;

    if (o->type == T_ARRAY) {
        int i;
        for (i = o->user.location.pos; i < ary_len(o); i++) {
            obj *k = ary_item(o,i);
            int left = obj_write(k,fd);
            if (left != 0) {
                return left;
            } else {
                o->user.location.pos++;
            }
        }
        return 0;
    } else {
        u32 len = data_len(o);
        if (len == 0)
            return 0;

        rc = write(fd,data_ptr(o,o->user.location.pos), len - o->user.location.pos);
        if (rc > 0)
            o->user.location.pos += rc;
        return rc < 0 ? rc : len - o->user.location.pos;
    }
}

obj obj_serialize(obj *o) {
    return encode(o);
}

obj obj_deserialize(obj *o) {
    return decode(o);
}
