#include "view.h"
#define STATUS(c,n,content_type) status((c),200,(content_type),sizeof((content_type)))
static u8 T_OPEN_BEGIN[] = {'<'};
static u8 T_OPEN_END[] = {'>'};
static u8 T_CLOSE_BEGIN[] = {'<','/'};
static u8 T_CLOSE_END[] = {'>'};
static u8 CONNECTION_CLOSE[] = { 'C','o','n','n','e','c','t','i','o','n',':',' ','c','l','o','s','e','\r','\n' };
static u8 TEXT_HTML[] = { 'C','o','n','t','e','n','t','-','T','y','p','e',':',' ','t','e','x','t','/','h','t','m','l','\r','\n' };
static u8 TEXT_PLAIN[] = { 'C','o','n','t','e','n','t','-','T','y','p','e',':',' ','t','e','x','t','/','p','l','a','i','n','\r','\n' };
static u8 APPLICATION_JSON[] = { 'C','o','n','t','e','n','t','-','T','y','p','e',':',' ','a','p','p','l','i','c','a','t','i','o','n','/','j','s','o','n','\r' };
static u8 NEW_LINE[] = {'\r','\n'};
static u8 STATUS_200[] = {'H','T','T','P','/','1','.','1',' ','2','0','0',' ','O','K','\r','\n'};
static u8 STATUS_302[] = {'H','T','T','P','/','1','.','1',' ','3','0','2',' ','M','o','v','e','d','\r','\n'};
static u8 STATUS_404[] = {'H','T','T','P','/','1','.','1',' ','4','0','4',' ','n','o','t',' ','f','o','u','n','d','\r','\n'};

inline void h_open(obj *obj,char *tag) {
    data_concat(obj,T_OPEN_BEGIN,sizeof(T_OPEN_BEGIN));
    text(obj,tag);
    data_concat(obj,T_OPEN_END,sizeof(T_OPEN_END));
}

inline void h_close(obj *obj,char *tag) {
    data_concat(obj,T_CLOSE_BEGIN,sizeof(T_CLOSE_BEGIN));
    text(obj,tag);
    data_concat(obj,T_CLOSE_END,sizeof(T_CLOSE_END));
}

inline void text(obj *obj,char *t) {
    data_concat(obj,(u8 *) t,strlen(t));
}

void cookie(struct client *c) {
    obj *cookie = hash_get_raw(&c->input,T_DATA,(u8 *) "Cookie",6);
    if (!cookie || data_len(cookie) != sizeof(c->session.sid.bytes)) {
        generate_sid(&c->session);
    } else {
        memcpy(c->session.sid.bytes,data_ptr(cookie,0),data_len(cookie));
    }
    c->flags |= COOKIE;
}

void status(struct client *c, int status,u8 *header,u32 header_len) {
    switch(status) {
    case 200:
        data_concat(&c->output,STATUS_200,sizeof(STATUS_200));
        break;
    case 302:
        data_concat(&c->output,STATUS_302,sizeof(STATUS_200));
    case 404:
        data_concat(&c->output,STATUS_404,sizeof(STATUS_404));
        break;
    default:
        SAYX("unknown status: %d",status);
    }
    data_concat(&c->output,CONNECTION_CLOSE,sizeof(CONNECTION_CLOSE));
    data_concat(&c->output,header,header_len);
    if (c->flags & COOKIE) {
        data_concat(&c->output,(u8 *)"Set-Cookie: ",12);
        data_concat(&c->output,c->session.sid.bytes,sizeof(c->session.sid.bytes));
        data_concat(&c->output,NEW_LINE,sizeof(NEW_LINE));
    }
    data_concat(&c->output,NEW_LINE,sizeof(NEW_LINE));
}

void play(struct client *c) {
    cookie(c);
    STATUS(c,200,TEXT_HTML);
//    obj_dump(&c->input);
    if (c->purl.field_set & (1 << UF_QUERY)) {
        obj query = ary_new();
        data_split(&query,&c->url,c->purl.field_data[UF_QUERY].off,c->purl.field_data[UF_QUERY].len,'&','=');
//        obj_dump(&query);
//        obj_dump(hash_get_raw(&query,T_DATA,"a",1));
        obj_destroy(&query);

    }

        obj s = obj_serialize(&c->input);
        obj_dump(&s);
        obj d = obj_deserialize(&s);
        obj_dump(&d);
        obj_destroy(&d);
        obj_destroy(&s);
    h_open(&c->output,"html");
    h_open(&c->output,"center");
    text(&c->output,"hello world");
    h_close(&c->output,"center");
    h_close(&c->output,"html");
//    obj_dump(&c->output);
}

#undef STATUS
