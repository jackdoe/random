typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
struct _b {
    u8 *value;
    u64 pos;
    u64 len;
};
typedef struct _b blob;

blob * new_blob(u64 len);
blob * set_blob(blob *b, u8 *data, u64 len);
blob * reset_blob(blob *b);
blob * merge_blob(blob *a, blob *b);
void destroy_blob(blob *b);
int cmp_blob(blob *a, blob *b);

int cmp_blob(blob *a, blob *b) {
    if (a->len != b->len)
        return 0;
    u64 p;
    for (p = 0;p < a->len; p++) {
        if (a->value[p] != b->value[p])
            return 0;
    } 
    return 1;
}
blob * reset_blob(blob *b) {
    b->pos = 0;
    return b;
}
blob * set_blob(blob *b, u8 *data, u64 len) {
    u64 copy_len = MIN(len,(b->len - b->pos));
    COPY(data,(b->value + b->pos),copy_len);
    b->pos += copy_len;
    return b;
}
blob * append_blob(blob *b, u8 *data, u64 len) {
    if ((b->pos + len) > b->len) {
        blob *m = new_blob(b->pos + len + 1);
        set_blob(m,b->value,b->pos);
        set_blob(m,data,len);
        destroy_blob(b);
        return m;
    } else {
        set_blob(b,data,len);
        return b;
    }
}
blob * merge_blob(blob *a, blob *b) {
    blob *m = new_blob(a->len + b->len);
    set_blob(m,a->value,a->len);
    set_blob(m,b->value,b->len);
    destroy_blob(a);
    destroy_blob(b);
    return m;
}
void destroy_blob(blob *b) {
    free(b->value);
    free(b);
}
blob * new_blob(u64 len) {
    assert(len > 0);
    blob *b = malloc(sizeof(*b));
    assert(b);
    b->value = malloc(sizeof(*b->value) * len);
    assert(b->value);
    b->len = len;
    b->pos = 0;
    return b;
}
