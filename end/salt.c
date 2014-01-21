#include "salt.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/poll.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <sys/types.h>
#include <signal.h>
#include <limits.h>
#include <sys/wait.h>

#define MAX_EVENTS 128

#define SAYPX(fmt,arg...) SAYX(fmt " { %s }",##arg,errno ? strerror(errno) : "undefined error");

struct worker {
    pthread_t tid;
    int efd;
    int flags;
    struct epoll_event events[MAX_EVENTS];
};

static int LISTEN_SOCK;
volatile int ABORT = 0;

void __die(int sig) {
    shutdown(LISTEN_SOCK,SHUT_RDWR);
    D("bye handler");
    ABORT = 1;
}

int header_field_cb (http_parser *p, const char *buf, size_t len);
int header_value_cb (http_parser *p, const char *buf, size_t len);
int body_cb (http_parser *p, const char *buf, size_t len);
int message_complete_cb (http_parser *p);
int url_cb(http_parser *p, const char *buf, size_t len);

static http_parser_settings settings = {
    .on_message_begin = NULL,
    .on_header_field = header_field_cb,
    .on_header_value = header_value_cb,
    .on_url = url_cb,
    .on_body = body_cb,
    .on_headers_complete = NULL,
    .on_message_complete = message_complete_cb
};

inline void ev_set(int efd, int cmd, int fd, int events, void *dptr) {
    struct epoll_event ev;
    ev.events = events;
    ev.data.ptr = dptr;
    if (epoll_ctl(efd, cmd, fd,&ev) == -1)
        SAYPX("epoll_ctl failed on efd: %d for fd %d cmd: %d",efd,fd,cmd);
}

inline void cleanup(struct client *c) {
    epoll_ctl(c->efd,EPOLL_CTL_DEL,c->fd,NULL);
    shutdown(c->fd,SHUT_RDWR);
    close(c->fd);
    obj_destroy(&c->input);
    obj_destroy(&c->url);
    obj_destroy(&c->output);
    free(c);
}

void dump_url (const char *url, const struct http_parser_url *u)
{
    unsigned int i;

    printf("\tfield_set: 0x%x, port: %u\n", u->field_set, u->port);
    for (i = 0; i < UF_MAX; i++) {
        if ((u->field_set & (1 << i)) == 0) {
            printf("\tfield_data[%u]: unset\n", i);
            continue;
        }

        printf("\tfield_data[%u]: off: %u len: %u part: \"%.*s\"\n",
               i,
               u->field_data[i].off,
               u->field_data[i].len,
               u->field_data[i].len,
               url + u->field_data[i].off);
    }
}

inline void __c_read(struct worker *self,struct client *c) {
    size_t len,nparsed;
    char buf[BUFSIZ];
    do {
        len = read(c->fd,buf,sizeof(buf));
        if (c->flags & MESSAGE_COMPLETE) {
            D("more input on completed request, ignoring %zu bytes",len);
        } else {
            if (len == -1) {
                if (len == EWOULDBLOCK || len == EAGAIN)
                    break;
                else {
                    cleanup(c);
                    break;
                }
            }
            nparsed = http_parser_execute(&c->http_parser, &settings, buf, len);
            if (nparsed != len) {
                D("nparsed %zu != %zu %s",nparsed,len, http_errno_description(HTTP_PARSER_ERRNO(&c->http_parser)));
                cleanup(c);
                break;
            }
            if (c->flags & MESSAGE_COMPLETE) {
                http_parser_parse_url((char *) data_ptr(&c->url,0),data_len(&c->url),0,&c->purl);
                play(c);
                ev_set(self->efd,EPOLL_CTL_MOD,c->fd,EPOLLOUT|EPOLLET,c);
                break;
            }
        }
    } while(len > 0);
}

inline void __c_write(struct worker *self,struct client *c) {
    int left;
    do {
        left = obj_write(&c->output,c->fd);
        if (left < 0) {
            if (left == -1 && (left == EWOULDBLOCK || left == EAGAIN))
                return;
            D("transmit failed %s",strerror(errno));
            break;
        }
    } while (left > 0);
    cleanup(c);
}

void *__worker(void *arg) {
    struct worker *self = (struct worker *) arg;
    int nfds,i;
    struct client *c;
    
    while(!ABORT) {
        nfds = epoll_wait(self->efd, self->events, MAX_EVENTS, 1000);
        if (nfds == -1) {
            D("epoll_wait on efd: %d returned %d",self->efd,nfds);
            // break;
        }
        for (i = 0; i < nfds; i++) {
            c = (struct client *) self->events[i].data.ptr;
            if (self->events[i].events & EPOLLIN)
                __c_read(self,c);
            if (self->events[i].events & EPOLLOUT)
                __c_write(self,c);
        }
        
    }

    D("worker died");
    return NULL;
}

inline void c_init(struct worker *w, int fd) {
    struct client *c = x_malloc(sizeof(*c));

    http_parser_init(&c->http_parser, HTTP_REQUEST);
    c->http_parser.data = c;
    c->fd = fd;
    c->efd = w->efd;
    c->flags = 0;
    obj_init(&c->input,T_ARRAY);
    obj_init(&c->output,T_DATA);
    obj_init(&c->url,T_DATA);

    ev_set(w->efd,EPOLL_CTL_ADD,fd,EPOLLIN|EPOLLET,c);
}

int socketize(char *ip,uint16_t port, int n_workers) {
    int fd,optval = 1;
    struct sockaddr_in laddr;
    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        SAYPX("socket");

    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &optval,sizeof(optval)) < 0)
        SAYPX("setsockopt");

    memset(&laddr, 0, sizeof(laddr));
    laddr.sin_family = AF_INET;
    if (!inet_aton(ip, &laddr.sin_addr))
        SAYPX("inet_aton failed on %s",ip);

    laddr.sin_port = htons(port);

    if (bind(fd, (struct sockaddr *) &laddr, sizeof(laddr)) < 0 ) 
        SAYPX("bind");

    if (listen(fd, MAX_EVENTS * n_workers) < 0)
        SAYPX("listen");

    D("listening: on %s:%d with", ip,port);
    return fd;
}

inline void set_non_blocking(int fd) {
    char optval = 1;
    if (ioctl(fd, FIONBIO, &optval))
        SAYPX("ioctl - set non blocking");
}

void server(char *ip, uint16_t port, int n_workers) {
    struct worker *w;
    signal(SIGINT, __die);
    signal(SIGTERM, __die);
    struct worker workers[n_workers];
    int conn_sock,i,universal_selector = 0;
    for (i = 0; i < n_workers; i++) {
        w = &workers[i];
        bzero(w,sizeof(*w));
        w->efd = epoll_create(MAX_EVENTS);
        if (w->efd == -1)
            SAYX("failed to create epoll");
        pthread_create(&w->tid,NULL,__worker,w);
    }

    LISTEN_SOCK = socketize(ip,port, n_workers);
    for(;;) {
        conn_sock = accept(LISTEN_SOCK,NULL, NULL);
        if (conn_sock < 0)
            break;
        set_non_blocking(conn_sock);
        c_init(&workers[universal_selector++ % n_workers],conn_sock);
    }

    D("main accept loop died");
    for (i = 0; i < n_workers; i++) {
        w = &workers[i];
        pthread_join(w->tid, NULL);
    }
}

// "./static/img/../lib/../././././../static/img/favicon.gif" -> 'static/img/favicon.gif'
inline void normalize_bang(char *path) {
    char *stack[PATH_MAX];
    char *token, *save,*begin;
    size_t pos = 0;
    for(begin = path;(token = strtok_r(begin, "/", &save)); begin = save) {
        if (token[0] == '.' && strcmp(token,"..") == 0) {
            stack[pos] = 0;
            if (pos > 0)
                pos--;
        } else {
            if (token[0] != '.' && pos < PATH_MAX) {
                stack[pos] = token;
                pos++;
            }
        }
    }
    int i,offset = 0,len;

    for (i = 0; i < pos; i++) {
        len = strlen(stack[i]);
        memcpy(path + offset,stack[i],len);
        path[offset + len] = '/';
        offset += len + 1;
    }
    path[offset > 0 ? offset - 1 : 0] = '\0';
}

#define INPUT_APPEND(c,buf,len,flag)                            \
    do {                                                        \
        obj *f = ary_last(&c->input);                           \
        if (!f || (f->flags & flag) == 0) {                     \
            obj value = data_new();                           \
            value.flags |= flag;                                \
            ary_push(&c->input,&value);                         \
        }                                                       \
        data_concat(ary_last(&c->input),(u8 *) buf,len);      \
    } while(0);

int header_field_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    INPUT_APPEND(c,buf,len,HEADER);
    return 0;
}

int header_value_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    INPUT_APPEND(c,buf,len,VALUE);
    return 0;
}


int body_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    INPUT_APPEND(c,buf,len,BODY);
    return 0;
}


int message_complete_cb (http_parser *p) {
    struct client *c = (struct client *) p->data;
    c->flags |= MESSAGE_COMPLETE;
    return 0;
}

int url_cb(http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    data_concat(&c->url,(u8 *) buf,len);
    return 0;
}
#undef INPUT_APPEND
