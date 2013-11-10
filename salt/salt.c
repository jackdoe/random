// #include <ruby-1.9.1/ruby.h>
// #include <ruby.h>/
#include <EXTERN.h>
#include <perl.h>
#include <time.h>
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
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <signal.h>
#include <sys/inotify.h>
#include <limits.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <sys/wait.h>
#include "http-parser/http_parser.c"
EXTERN_C void xs_init (pTHX); // http://perldoc.perl.org/perlembed.html

#define WORKERS 4
#define MAX_EVENTS 768

#define FNV_PRIME_32 16777619
#define FNV_OFFSET_32 2166136261U

#define INOTIFY_EVENT_LEN ( sizeof( struct inotify_event ) )
#define INOTIFY_BUF_LEN ( 8192 * ( INOTIFY_EVENT_LEN + 16 ) )
#define HASH_BITS 16
#define HASH_MASK 0xFFFF
#define HASH_SIZE HASH_MASK + 1

#define FORMAT(fmt,arg...) fmt " [%s():%s:%d @ %u]\n",##arg,__func__,__FILE__,__LINE__,(unsigned int) time(NULL)
#define E(fmt,arg...) fprintf(stderr,FORMAT(fmt,##arg))
#define D(fmt,arg...) printf(FORMAT(fmt,##arg))
#define SAYX(rc,fmt,arg...) do {    \
    E(fmt,##arg);                   \
    exit(rc);                       \
} while(0)

#define SAYPX(fmt,arg...) SAYX(EXIT_FAILURE,fmt " { %s }",##arg,errno ? strerror(errno) : "undefined error");
#define REQUEST_CHECKED_FOR_STATIC_FILE_MATCH 4
#define SOCKET_EPOLLED 8
#define SSL_WOULD_BLOCK_ACCEPT_AGAIN 16
#define SOCKET_IS_SSL 32
#define WAITING_FOR_INTERPRETER 64
#define DESTROY_AFTER_INTERPRETER_IS_DONE 128
#define WANT_TO_WRITE 1
#define DONE_WRITING 2
#define IS_CACHED 1
#define __GET_METHOD 0x20544547 // {'G','E','T',' '}
#define NOT_EAGAIN(x) ((x) == -1 && errno != EWOULDBLOCK && errno != EAGAIN)
#define D_PKT_SIZE(x) ((x)->len + sizeof(struct data))
#define D_PTR(x,off) ((x)->payload + off)
#define D_CURRENT_PTR(x) D_PTR((x),(x)->len)
struct data {
    unsigned char flags;
    size_t len;
    size_t pos;
    size_t size;
    size_t pending;
    size_t body_offset;
    struct http_parser_url url;
    void *ptr;
    char payload[0];
}__attribute__((packed));

struct cached {
    char *path;
    int wd;
    struct data *data;
    struct cached *next;
    struct cached *head;
};
struct cached F_CACHE[HASH_SIZE];
struct worker {
    pthread_t tid;
    int epollfd;
    int inotify;
    int flags;
    struct epoll_event events[MAX_EVENTS];
    SSL_CTX *ctx;
    int pipe_in[2];
    int pipe_out[2];
    int pos;
};

struct client {
    int fd;
    size_t output_pos;
    struct worker *worker;
    http_parser http_parser;
    char flags;
    struct data *input;
    struct data *output;
    SSL *ssl;
} __attribute__((packed));

static int LISTEN_SOCK,LISTEN_SOCK_SSL;
static struct worker workers[WORKERS];
char NEW_LINE[] = {'\r','\n'};
volatile int ABORT = 0;
static PerlInterpreter *my_perl;
void __die(int sig) {
    if (sig == SIGCHLD)
        D("child exit");
    shutdown(LISTEN_SOCK,SHUT_RDWR);
    shutdown(LISTEN_SOCK_SSL,SHUT_RDWR);
    ABORT = 1;
}

int header_field_cb (http_parser *p, const char *buf, size_t len);
int header_value_cb (http_parser *p, const char *buf, size_t len);
int body_cb (http_parser *p, const char *buf, size_t len);
int request_url_cb (http_parser *p, const char *buf, size_t len);
int message_complete_cb (http_parser *p);
struct data * depipe(int fd);
int enpipe(int fd, struct data *d);
void __fork_interpreter(struct worker *w);
void cache_reset(void);
int send_file_if_static(struct client *c, char *buf, size_t len);
struct client * __c_accept_ssl(struct client *);
void error(struct client *c, int code);
void recursive_cache(const char *root, const char * dir_name);
void cache_store_file(char *http_path,char *path);
void send_header_with_type_and_length(struct data **d,const char *content_type, size_t len);

static http_parser_settings settings = {
    .on_message_begin = NULL,
    .on_header_field = header_field_cb,
    .on_header_value = header_value_cb,
    .on_url = request_url_cb,
    .on_body = body_cb,
    .on_headers_complete = NULL,
    .on_message_complete = message_complete_cb
};

struct CRYPTO_dynlock_value { 
    pthread_mutex_t mutex; 
}; 

static pthread_mutex_t *mutex_buf = NULL; 

static void locking_function(int mode, int n, const char *file, int line) { 
    if (mode & CRYPTO_LOCK) { 
        pthread_mutex_lock(&mutex_buf[n]); 
    } else { 
        pthread_mutex_unlock(&mutex_buf[n]); 
    } 
} 

static unsigned long id_function(void)  { 
    return ((unsigned long) pthread_self()); 
} 

static struct CRYPTO_dynlock_value *dyn_create_function(const char *file, int line) { 
    struct CRYPTO_dynlock_value *value; 

    value = (struct CRYPTO_dynlock_value *) 
        malloc(sizeof(struct CRYPTO_dynlock_value)); 
    if (!value) { 
        goto err; 
    } 
    pthread_mutex_init(&value->mutex, NULL); 

    return value; 

  err: 
    return (NULL); 
} 

static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value *l, const char *file, int line) {
    if (mode & CRYPTO_LOCK) { 
        pthread_mutex_lock(&l->mutex); 
    } else { 
        pthread_mutex_unlock(&l->mutex); 
    } 
} 

static void dyn_destroy_function(struct CRYPTO_dynlock_value *l, const char *file, int line) { 
    pthread_mutex_destroy(&l->mutex); 
    free(l); 
} 

inline void x_free(void *c) {
    if (c)
        free(c);
}

inline void *x_realloc(void *c, ssize_t len) {
    // D("realloc %zu",len);
    c =  realloc(c,len);
    if (!c)
        SAYX(EXIT_FAILURE,"not enough memory to allocate %zu bytes",len);
    return c;
}

inline void *x_malloc(ssize_t len) {
    return x_realloc(NULL,len);
}
struct data *d_init(void *ptr) {
    struct data *d = x_malloc(sizeof(*d));
    bzero(d,sizeof(*d));
    d->ptr = ptr;
    return d;
}

inline void d_append(struct data **d, const char *bytes, size_t len) {
    if ((*d)->size < (*d)->len + len) {
        size_t size = D_PKT_SIZE(*d) + len + 8192;
        // D("appending size: %zu  -> %zu bytes",(*d)->size,size);
        *d = x_realloc((*d),size);
        (*d)->size = size;
    } else {
        // D("no realloc needed %zu > %zu",(*d)->size,len);
    }
    if (bytes != NULL)
        memcpy(D_CURRENT_PTR(*d),bytes,len);

    (*d)->len += len;
    // D("updated len: %zu",(*d)->len);
}

inline void d_concat(struct data **big, struct data **small) {
    d_append(big,D_PTR(*small,0),(*small)->len);
}

inline void d_destroy(struct data **d) {
    x_free(*d);
    *d = NULL;
    // D("destroy");
}

inline void ev_set(int efd, int cmd, int fd, int events, void *dptr) {
    struct epoll_event ev;
    ev.events = events;
    ev.data.ptr = dptr;
    if (epoll_ctl(efd, cmd, fd,&ev) == -1)
        SAYPX("epoll_ctl failed on epollfd: %d for fd %d cmd: %d",efd,fd,cmd);
}

void cleanup(struct client *c) {
    // D("cleanup");
    if (c->flags & WAITING_FOR_INTERPRETER) {
        D("attempting to destroy waiting for interpreter connection");
        return;
    }

    if (c->flags & SOCKET_EPOLLED)
        epoll_ctl(c->worker->epollfd,EPOLL_CTL_DEL,c->fd,NULL);

    shutdown(c->fd,SHUT_RDWR);
    close(c->fd);
    if (c->ssl) {
        SSL_free(c->ssl);
        D("cleanup");
    }
    d_destroy(&c->input);
    if (c->output && (c->output->flags & IS_CACHED) == 0)
        d_destroy(&c->output);
    x_free(c);
}
inline int is_worker_busy(struct worker *w) {
    return (w->flags & WAITING_FOR_INTERPRETER);
}
void request_for_interpreter(struct client *c,int what) {
    struct worker *w = c->worker;
    if (what == WANT_TO_WRITE) {
        if (is_worker_busy(c->worker)) {
            error(c,503);
        } else {        
            c->flags |= WAITING_FOR_INTERPRETER;
            w->flags |= WAITING_FOR_INTERPRETER;
            ev_set(w->epollfd,EPOLL_CTL_ADD,w->pipe_in[1],EPOLLOUT,c);      
        }       
    } else {
        c->flags &= ~WAITING_FOR_INTERPRETER;
        w->flags &= ~WAITING_FOR_INTERPRETER;
        ev_set(w->epollfd,EPOLL_CTL_DEL,w->pipe_in[1],0,c);     
    }
}

inline void request_for_write(struct client *c,int what) {
    int events = 0;
    c->input->pos = 0;
    if (what == WANT_TO_WRITE) {
        events |= EPOLLOUT;
    } else {
        events &= ~EPOLLOUT;
    }
    ev_set(c->worker->epollfd,EPOLL_CTL_MOD,c->fd,events|EPOLLIN,c);
}

#define CLEAN_IF_NOT_WAITING                        \
if (len < 1) {                                      \
    if (NOT_EAGAIN(len) || len == 0)                \
        cleanup(c);                                 \
    D("would block %s",strerror(errno));            \
    return;                                         \
}

void __c_read(struct worker *self,struct client *c) {
    size_t len = 0,nparsed;
    char buf[BUFSIZ];
    if (c->ssl) {
        if (c->flags & SSL_WOULD_BLOCK_ACCEPT_AGAIN) {
            c = __c_accept_ssl(c);
            if (!c)
                return;
        } else {
            len = SSL_read(c->ssl,buf,sizeof(buf));
        }
    } else {
        len = read(c->fd,buf,sizeof(buf));
    }
    CLEAN_IF_NOT_WAITING;
    send_file_if_static(c,"/main.rb", 8);
    return;
    // send_file_if_static
    // send_file_if_static(c,"/main.rb", 8);
    // return;

    // connection is waiting for data from interpreter
    // nothing to do, just ignore the input
    if (c->flags & WAITING_FOR_INTERPRETER) {
        D("connection is waiting for data from interpreter, we should not receive data, but we did: %zu",len);
    } else {
        nparsed = http_parser_execute(&c->http_parser, &settings, buf, len);
        if (nparsed != len) {
            D("nparsed %zu != %zu %s",nparsed,len, http_errno_description(HTTP_PARSER_ERRNO(&c->http_parser)));
            cleanup(c);
        }   
    }
    // if (!(c->flags & REQUEST_CHECKED_FOR_STATIC_FILE_MATCH)) {
    //  // D("checking for get %x",__GET_METHOD);
    //  c->flags |= REQUEST_CHECKED_FOR_STATIC_FILE_MATCH;
    //  if (len > sizeof(uint32_t) && (* (uint32_t *) buf) == __GET_METHOD) {
    //      // D("IS GET");
    //      // char *uri = buf + sizeof(uint32_t);
    //      // len -= sizeof(uint32_t);

    //      // char *space = memrchr(uri,' ',len);
    //      // if (space)
    //      //  len = space - uri;
    //      // dump(uri,len);
    //      // dump(buf,len);
    //      // if (memcmp(buf,"keep-alive",len) == 0 || memcmp(buf,"Keep-Alive",len) == 0 || memcmp(buf,"HTTP/1.1",len) == 0) {
    //      //  c->flags |= KEEPALIVE;
    //      //  D("KEEPALIVE");
    //      // }
    //      if (send_file_if_static(self,c,"/main.rb", 8) == 0)
    //          continue;
    //  }
    //  D("NOP");
    // }

    // if (NIL_P(c->env))
    //  c->env = rb_hash_new();
}
void __c_write(struct worker *self,struct client *c) {
    int len;
    if (c->flags & WAITING_FOR_INTERPRETER) {
        enpipe(c->worker->pipe_in[1],c->input);
        if (c->input->pending == 0)
            request_for_interpreter(c,DONE_WRITING);
    } else {
        if (!c->output ||  c->output->len <= 0)
            SAYX(EXIT_FAILURE,"epollout event without output len or output object %p",c);

        if (c->ssl) {
            len = SSL_write(c->ssl,D_PTR(c->output,c->output_pos),c->output->len);
        } else {
            len = send(c->fd,D_PTR(c->output,c->output_pos),c->output->len,MSG_DONTWAIT|MSG_NOSIGNAL);
        }
        if (len > 0)
            c->output_pos += len;

        CLEAN_IF_NOT_WAITING;

        if (c->output_pos == c->output->len)
            cleanup(c);
    }
}
void *__worker(void *arg) {
    struct worker *self = (struct worker *) arg;
    D("worker spawned, with efd: %d",self->epollfd);
    int nfds,i;
    struct client *c;
    while(!ABORT) {
        // D("waiting on %d",self->epollfd);
        nfds = epoll_wait(self->epollfd, self->events, MAX_EVENTS, 1000);
        if (nfds == -1) {
            D("epoll_wait on efd: %d returned %d",self->epollfd,nfds);
            // break;
        }
        // D("nfds: %d",nfds);
        for (i = 0; i < nfds; i++) {
            if (self->events[i].data.ptr == self) {
                struct data *d = depipe(self->pipe_out[0]);
                c = (struct client *) d->ptr;
                d_destroy(&c->output);
                c->output = d;
                c->output_pos = 0;
                d->pos = 0;
                request_for_write(c,WANT_TO_WRITE);
            } else {
                c = (struct client *) self->events[i].data.ptr;
                if (self->events[i].events & EPOLLIN) {
                    __c_read(self,c);
                }
                // if ((self->events[i].events & EPOLLOUT)) {
                //     __c_write(self,c);
                // }
            }
        }
    }
    D("worker died");
    return NULL;
}

#undef CLEAN_IF_NOT_WAITING
struct client * __c_accept_ssl(struct client *c) {  
    int rc = SSL_accept(c->ssl);
    if (rc == 1) {
        c->flags &= ~SSL_WOULD_BLOCK_ACCEPT_AGAIN;
    } else if (rc == -1) {
        int err = SSL_get_error(c->ssl,rc);
        if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE) {
            c->flags |= SSL_WOULD_BLOCK_ACCEPT_AGAIN;
        } else {
            cleanup(c);
            return NULL;
        }
    } else if (!(NOT_EAGAIN(rc))) {
        D("would block-1");
        c->flags |= SSL_WOULD_BLOCK_ACCEPT_AGAIN;
    }
    return c;
}

void c_init(struct worker *w, int fd,int flags) {
    struct client *c = x_malloc(sizeof(*c));
    bzero(c,sizeof(*c));
    http_parser_init(&c->http_parser, HTTP_REQUEST);
    c->http_parser.data = c;
    c->flags = flags;
    c->fd = fd;
    c->worker = w;
    c->output_pos = 0;
    c->output = d_init(c);
    c->input = d_init(c);
    if (c->flags & SOCKET_IS_SSL) {
        c->ssl = SSL_new(w->ctx);
        if (!c->ssl)
            SAYX(EXIT_FAILURE,"failed to create ssl from context");
        if (SSL_set_fd(c->ssl, fd) == 0)
            SAYX(EXIT_FAILURE,"SSL_set_fd failed");
        c = __c_accept_ssl(c);
        if (!c)
            return;
    }
    c->flags |= SOCKET_EPOLLED;
    ev_set(w->epollfd,EPOLL_CTL_ADD,fd,EPOLLIN,c);
}

int socketize(char *ip,uint16_t port) {
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

    if (listen(fd, MAX_EVENTS * WORKERS) < 0)
        SAYPX("listen");

    D("listening: on %s:%d with", ip,port);
    return fd;
}

void set_non_blocking(int fd) {
    char optval = 1;
    if (ioctl(fd, FIONBIO, &optval))
        SAYPX("ioctl - set non blocking");
}

void __server(char *ip, uint16_t port) {
    LISTEN_SOCK = socketize(ip,port);
    LISTEN_SOCK_SSL = socketize(ip,port + 1);
    int conn_sock,i = 0,j, efd,nfds,universal_selector = 0;
    efd = epoll_create(MAX_EVENTS);
    if (efd == -1)
        SAYPX("failed to create epoll");
    struct epoll_event events[MAX_EVENTS];
    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.fd = LISTEN_SOCK;
    if (epoll_ctl(efd, EPOLL_CTL_ADD, LISTEN_SOCK,&ev) == -1)
        SAYPX("epoll_ctl");

    ev.data.fd = LISTEN_SOCK_SSL;
    if (epoll_ctl(efd, EPOLL_CTL_ADD, LISTEN_SOCK_SSL,&ev) == -1)
        SAYPX("epoll_ctl");
    int flags = 0;
    for(;;) {
        nfds = epoll_wait(efd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            D("epoll_wait on efd: %d returned %d",efd,nfds);
            break;
        }
        for (i = 0; i < nfds; i++) {
            int fd = events[i].data.fd;
            conn_sock = accept(fd,NULL, NULL);
            if (NOT_EAGAIN(conn_sock))
                SAYPX("accept");

            set_non_blocking(conn_sock);
            flags = (fd == LISTEN_SOCK_SSL) ? SOCKET_IS_SSL : 0;
            // just try to pick the first non busy worker
            // or if all of them are busy just pick the last one
            // so he can return 503
            for (j = 0; j < WORKERS; j++) {
                struct worker *w = &workers[universal_selector++ % WORKERS];
                if (!is_worker_busy(w) || j == WORKERS - 1) {
                     c_init(w,conn_sock,flags);
                     break;
                }
            }
        }
    }
    D("main accept loop died");
}


int main(int argc, char **argv) {
    if (argc != 3)
        SAYX(EXIT_FAILURE,"usage: %s static_file_root file.rb (that contains Application class)",argv[0]);
    recursive_cache(argv[1],argv[1]);

    struct worker *w;
    int i;

    mutex_buf = x_malloc(CRYPTO_num_locks() * sizeof(pthread_mutex_t)); 

    for (i = 0; i < CRYPTO_num_locks(); i++) { 
        pthread_mutex_init(&mutex_buf[i], NULL); 
    } 
    CRYPTO_set_locking_callback(locking_function); 
    CRYPTO_set_id_callback(id_function); 
    CRYPTO_set_dynlock_create_callback(dyn_create_function); 
    CRYPTO_set_dynlock_lock_callback(dyn_lock_function); 
    CRYPTO_set_dynlock_destroy_callback(dyn_destroy_function); 
    const SSL_METHOD *method;
    SSL_library_init();
    OpenSSL_add_all_algorithms();
    SSL_load_error_strings();
    method = SSLv23_server_method();
    for (i = 0; i < WORKERS; i++) {
        w = &workers[i];
        bzero(w,sizeof(*w));
        w->epollfd = epoll_create(MAX_EVENTS);
        if (w->epollfd == -1)
            SAYPX("failed to create epoll");

        w->ctx = SSL_CTX_new(method);
        if (!w->ctx)
            SAYX(EXIT_FAILURE,"unable to create ssl context - %s",ERR_reason_error_string(ERR_get_error()));
        
        const char file[] = "mycert.pem";
        SSL_CTX_use_certificate_file(w->ctx, file, SSL_FILETYPE_PEM);
        SSL_CTX_use_PrivateKey_file(w->ctx, file, SSL_FILETYPE_PEM);
        if (!SSL_CTX_check_private_key(w->ctx))
            SAYX(EXIT_FAILURE,"ssl private key check failed");
        __fork_interpreter(w);
    }

    signal(SIGINT, __die);
    signal(SIGTERM, __die);
    signal(SIGCHLD, __die);
    for (i = 0 ;i < WORKERS; i++) {
        w = &workers[i];
        pthread_create(&w->tid,NULL,__worker,w);
    }

    // // int fd = open("./main.rb",O_RDONLY);
    // struct stat s;
    // if (fstat(fd, &s) == 0) {
    //      __data = mmap(NULL,s.st_size,PROT_READ,MAP_PRIVATE,fd,0);
    //      if (__data == MAP_FAILED)
    //          SAYPX("mmap");
    //      __len = s.st_size;
    //      // d_append(&c->output,data,s.st_size);
    //      // munmap(data,s.st_size);
    //      // c->flags |= SHUTDOWN_AFTER_SENDING_EVERYTHING;
    //      // request_for_write(c,WANT_TO_WRITE);
    //  // }
    //  // close(fd);
    // }

    __server("0.0.0.0",9999);
    for (i = 0; i < WORKERS; i++) {
        w = &workers[i];
        pthread_join(w->tid, NULL);
        close(w->pipe_in[0]);
        close(w->pipe_out[0]);
        close(w->pipe_in[1]);
        close(w->pipe_out[1]);
    }
    int status;
    while(wait(&status) > 0) { 
        ; 
    }
    cache_reset();
    return 0;
}

void send_http_status(struct data **d,int code,char c1, char c2) {
    if (code > 999 || code < 100)
        SAYX(EXIT_FAILURE,"status code must be >= 100 and <= 999");

    char STATUS[] = {'H','T','T','P','/','1','.','1',' ','Z','Z','Z',' ','Z','Z','\r','\n'};
    STATUS[9]  = (char)('0' + ((code/100) % 10));
    STATUS[10] = (char)('0' + ((code/10) % 10));
    STATUS[11] = (char)('0' + (code % 10));
    STATUS[13] = c1;
    STATUS[14] = c2;
    d_append(d,STATUS,sizeof(STATUS));
}
void send_http_nl(struct data **d) {
    d_append(d,NEW_LINE,sizeof(NEW_LINE));
}
void send_http_hdr(struct data **d, const char *key, const char *value) {
    d_append(d,key,strlen(key));
    d_append(d,": ",2);
    d_append(d,value,strlen(value));
    send_http_nl(d);
}

void send_http_end_of_header(struct data **d) {
    send_http_nl(d);
}
send_http_error(struct data **d, int code) {
    send_http_status(d,code,'E','R');
    send_http_end_of_header(d);    
}
void error(struct client *c, int code) {
    send_http_error(&c->output,code);
    request_for_write(c,WANT_TO_WRITE);
    D("error %d",code);
}

void cache_reset(void) {
    int i;
    for (i = 0; i < HASH_SIZE; i++) {
        struct cached *list = &F_CACHE[i],*e;
        while ((e = list->head)) {
            d_destroy(&e->data);
            x_free(e->path);
            list->head = e->next;
            x_free(e);
        }
    }
}

inline uint32_t FNV32(const char *s, size_t len) {
    uint32_t hash = FNV_OFFSET_32, i;
    for(i = 0; i < len; i++) {
        hash = hash ^ (s[i]);
        hash = hash * FNV_PRIME_32;
    }
    return (hash >> HASH_BITS) ^ (hash & HASH_MASK);
}

inline struct cached *cached_file(const char *relative_path,size_t len) {
    struct cached *cache,*list = &F_CACHE[FNV32(relative_path,len)];
    for (cache = list->head; cache ; cache = cache->next) {
        if (strcmp(relative_path,cache->path) == 0) {
             return cache;
        }
    }
    return NULL;
}

// "./static/img/../lib/../././././../static/img/favicon.gif" -> 'static/img/favicon.gif'
inline void normalize_bang(char *path) {
    char *stack[PATH_MAX];
    char *token, *save,*begin;
    size_t pos = 0;
    for(begin = path;token = strtok_r(begin, "/", &save); begin = save) {
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

void recursive_cache(const char *root, const char * dir_name) {
    DIR * d;
    if (!(d = opendir(dir_name))) 
        SAYPX("Cannot open directory '%s'",dir_name);
    int rlen = strlen(root);
    for(;;) {
        struct dirent * entry;
        if (!(entry = readdir(d)))
            break;

        // ignore hidden files/directories and . ..
        if (entry->d_name[0] == '.')
            continue;

        char path[PATH_MAX], http_path[PATH_MAX];
        int len = snprintf(path, PATH_MAX,"%s/%s", dir_name, entry->d_name);
        if (len > PATH_MAX)
            SAYX(EXIT_FAILURE,"path too long");

        if (entry->d_type & DT_DIR) {
            recursive_cache(root,path);
        } else if (entry->d_type & DT_REG) {
            int to_copy = len - rlen;
            memcpy(http_path,(path + rlen),to_copy);
            http_path[to_copy] = '\0';
            cache_store_file(http_path,path);
        }
    }
    if (closedir(d))
        SAYPX("Could not close '%s'",dir_name);
}

void cache_store_file(char *http_path,char *path) {
    normalize_bang(http_path);
    D("caching %s into http_path:%s",path,http_path);
    struct cached *entry,*list = &F_CACHE[FNV32(http_path,strlen(http_path))];

    entry = x_malloc(sizeof(*entry));
    bzero(entry,sizeof(*entry));
    entry->data = d_init(NULL);
    entry->data->flags |= IS_CACHED;
    entry->path = strdup(http_path);
    struct stat s;
    int fd = open(path,O_RDONLY);
    if (fd < 1)
        SAYPX("open");

    if (fstat(fd, &s) != 0)
        SAYPX("fstat");

    send_header_with_type_and_length(&entry->data,"octet/stream",s.st_size);
    size_t pos = entry->data->len;
    d_append(&entry->data,NULL,s.st_size); // make space
    if (read(fd,D_PTR(entry->data,pos),s.st_size) != s.st_size)
        SAYPX("read %s",path);
    close(fd);
    list->head = entry;
    entry->next = list->next;
}

void send_header_with_type_and_length(struct data **d,const char *content_type, size_t len) {
    char itoa[BUFSIZ];

    send_http_status(d,200,'O','K');
    snprintf(itoa,BUFSIZ,"%zu",len);
    send_http_hdr(d,"Content-Length",itoa);
    send_http_hdr(d,"Content-Type",content_type);
    send_http_hdr(d,"Connection","close");
    send_http_end_of_header(d);
}

int send_file_if_static(struct client *c, char *buf, size_t len) {
    if (len >= PATH_MAX)
        return -1;
    char path[PATH_MAX];
    memcpy(path,buf,len);
    path[len] = '\0';
    normalize_bang(path);

    struct cached *cache = cached_file(path,strlen(path));
    if (!cache)
        return -1;
    d_destroy(&c->output);
    c->output = cache->data;
    len = send(c->fd,D_PTR(cache->data,0),cache->data->len,MSG_DONTWAIT|MSG_NOSIGNAL);
    cleanup(c);
    // D("len: %zu total: %zu",len,cache->data->len);
    // request_for_write(c,WANT_TO_WRITE);
    return 0;
}

int header_field_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    // dump(buf,len);
    // D("AAA");
    d_append(&c->input,buf,len);
    // if (p->method) {
    //     D("method: %d",p->method);
    // }
    return 0;
}

int header_value_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    d_append(&c->input,buf,len);
    return 0;
}


int body_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    d_append(&c->input,buf,len);
    return 0;
}

int request_url_cb (http_parser *p, const char *buf, size_t len) {
    struct client *c = (struct client *) p->data;
    http_parser_parse_url(buf,len,1,&c->input->url);
    // d_append(&c->input,buf,len);
    c->input->body_offset = c->input->len;
    return 0;
}

int message_complete_cb (http_parser *p) {
    struct client *c = (struct client *) p->data;
    send_file_if_static(c,"/main.rb",8);
    // request_for_interpreter(c,WANT_TO_WRITE);
    return 0;
}

void dump(char *buf, size_t len) {
    int i;
    for (i = 0; i < len ; i++) {
        printf("%c",buf[i]);
    }
    printf("\n");
}

void instantiate(struct data **d, char *klass, size_t klen, HV *env) {
    dSP;
    ENTER;
    SAVETMPS;
    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSVpv(klass,klen)));
    XPUSHs(sv_2mortal(newRV_noinc((SV*)env)));
    PUTBACK;

    call_method("new", G_SCALAR);
    SPAGAIN;

    SV *app = POPs;
    PUTBACK;

    SPAGAIN;
    PUSHMARK(SP);
    XPUSHs(app);
    PUTBACK;    

    call_method("_call", G_SCALAR);
    SPAGAIN;
    SV *obj = POPs;

    STRLEN len;
    char *ptr = SvPV(obj, len);
    (*d)->len = 0;
    send_header_with_type_and_length(d,"octet/stream",len);
    d_append(d,ptr,len);

    PUTBACK;
    FREETMPS;
    LEAVE;
}
void __fork_interpreter(struct worker *w) {
    if (pipe(w->pipe_in) != 0 || pipe(w->pipe_out) != 0)
        SAYPX("pipe");

    pid_t pid = fork();
    if (pid < 0)
        SAYPX("fork");
    if (pid > 0) {
        // parent:
        // watch for interpreter output, and set our output to be nonblocking
        ev_set(w->epollfd,EPOLL_CTL_ADD,w->pipe_out[0],EPOLLIN,w);
        fcntl(w->pipe_in[1], F_SETFL, fcntl(w->pipe_in[1], F_GETFL) | O_NONBLOCK);
        return;
    }

    int in,out;
    in = w->pipe_in[0];
    out = w->pipe_out[1];

    char *embedding[] = { "", "-e", "0" };

    my_perl = perl_alloc();
    PERL_SET_CONTEXT(my_perl);
    perl_construct(my_perl);
    perl_parse(my_perl, xs_init, 3, embedding, (char **)NULL);
    perl_run(my_perl);
    eval_pv("use IO::String; use Data::Dumper;", TRUE);
    eval_pv("package Application; sub new {my $self =  bless { data => $_[1]}, 'Application'; return $self;} sub _call { my ($self) = @_; return Data::Dumper::Dumper($self) };", TRUE);
    for (;;) {
        struct data *d = depipe(in);
        d->pos = 0;
        d->pending = 0;

        ENTER;
        SAVETMPS;       
        HV *env = newHV();
        char *buf = d->payload;
        size_t len = d->body_offset;
        #define SET_KEY(key,value) hv_store(env,key,strlen(key),(value),0)
        #define SET_ENV(uf_flag, key)                                                                                \
        do {                                                                                                         \
            if (d->url.field_set & uf_flag) {                                                                        \
                SET_KEY(key,newSVpvn(d->payload + d->url.field_data[uf_flag].off,                                    \
                                                       d->url.field_data[uf_flag].len));                             \
            }                                                                                                        \
        } while (0);
        D("body_offset: %zu",d->body_offset);
        SET_ENV(UF_HOST,"HTTP_HOST");
        SET_ENV(UF_SCHEMA,"HTTP_SCHEME");
        SET_ENV(UF_PATH,"PATH_INFO");
        SET_ENV(UF_QUERY,"QUERY_STRING");
        SET_KEY("HTTP_BODY",newSVpvn(d->payload + d->body_offset,d->len - d->body_offset));
        instantiate(&d,"Application",strlen("Application"),env);
        #undef SET_ENV   
        #undef SET_KEY      
        // send_http_error(&d,401);
        enpipe(out,d);
        d_destroy(&d);

        FREETMPS;
        LEAVE;
    }

    perl_destruct(my_perl);
    perl_free(my_perl);
    D("my work is done.");
    exit(0);
}

int enpipe(int fd, struct data *d) {
    int rc;
    if (d->pending == 0) {
        d->pending = D_PKT_SIZE(d);
    }
    do {
        // D("BEFORE enpiped %d %zu",rc,d->pending);
        rc = write(fd,d,d->pending);
        if (NOT_EAGAIN(rc))
            SAYPX("enpipe failed with rc: %d",rc);
        if (rc > 0)
            d->pending -= rc;
        // D("enpiped %d %zu",rc,d->pending);
    } while (rc > 0 && d->pending == 0);
    return rc;
}

struct data * depipe(int fd) {
    struct data *p = x_malloc(sizeof(*p));
    int n = read(fd,p,sizeof(*p));
    if (n != sizeof(*p))
        SAYPX("read");

    p = x_realloc(p,D_PKT_SIZE(p));
    n = read(fd,D_PTR(p,0),p->len);
    if (n != p->len)
        SAYPX("read expected %zu got %d",p->len,n);
    p->size = D_PKT_SIZE(p) - sizeof(*p);
    return p;
}

// static VALUE mSalt;

// void init_module(void) {
//  mSalt = rb_define_module("Salt");
//  // eHttpParserError = rb_define_class_under(mThin, "InvalidRequest", rb_eIOError);
//  // DEF_GLOBAL(empty, "");
//  // DEF_GLOBAL(http_prefix, "HTTP_");
//  // DEF_GLOBAL(request_method, "REQUEST_METHOD");
//  // DEF_GLOBAL(request_uri, "REQUEST_URI");
//  // DEF_GLOBAL(fragment, "FRAGMENT");
//  // DEF_GLOBAL(query_string, "QUERY_STRING");
//  // DEF_GLOBAL(http_version, "HTTP_VERSION");
//  // DEF_GLOBAL(request_path, "REQUEST_PATH");
//  // DEF_GLOBAL(content_length, "CONTENT_LENGTH");
//  // DEF_GLOBAL(http_content_length, "HTTP_CONTENT_LENGTH");
//  // DEF_GLOBAL(content_type, "CONTENT_TYPE");
//  // DEF_GLOBAL(http_content_type, "HTTP_CONTENT_TYPE");
//  // DEF_GLOBAL(gateway_interface, "GATEWAY_INTERFACE");
//  // DEF_GLOBAL(gateway_interface_value, "CGI/1.2");
//  // DEF_GLOBAL(server_name, "SERVER_NAME");
//  // DEF_GLOBAL(server_port, "SERVER_PORT");
//  // DEF_GLOBAL(server_protocol, "SERVER_PROTOCOL");
//  // DEF_GLOBAL(server_protocol_value, "HTTP/1.1");
//  // DEF_GLOBAL(http_host, "HTTP_HOST");
//  // DEF_GLOBAL(port_80, "80");
//  // DEF_GLOBAL(http_body, "rack.input");
//  // DEF_GLOBAL(url_scheme, "rack.url_scheme");
//  // DEF_GLOBAL(url_scheme_value, "http");
//  // DEF_GLOBAL(script_name, "SCRIPT_NAME");
//  // DEF_GLOBAL(path_info, "PATH_INFO");
// }



// #define __FUNCALL_ARGC 3

// VALUE __rescue(VALUE data, VALUE exception)
// {
//     // do something...
//  VALUE rbError = rb_funcall(rb_gv_get("$!"), rb_intern("message"), 0);
//  if (TYPE(rbError) == T_STRING)
//      D("EXCEPTION: %s",StringValuePtr(rbError));
//     return Qnil;
// }
// VALUE __funcall_ex(VALUE data);
// VALUE __rescue(VALUE data, VALUE exception);

// VALUE __funcall(VALUE (*___method)(),VALUE receiver, ID method_id, int argc, ...) {
//     va_list arg_list;
 
//     long    argi;
//     VALUE   *argv;
 
//     VALUE   result = Qnil;
 
//     if( argc > 0 ) {
//         va_start(arg_list, argc);
 
//         argv = ALLOCA_N(VALUE, (argc + __FUNCALL_ARGC));
 
//         argv[0] = argc;
//         argv[1] = receiver;
//         argv[2] = method_id;
 
//         for( argi = 0; argi < argc; argi++ ) {
//             argv[argi + __FUNCALL_ARGC] = va_arg(arg_list, VALUE);
//         }
 
//         va_end(arg_list);
//     } else {
//         argv = ALLOCA_N(VALUE, __FUNCALL_ARGC);
 
//         argv[0] = argc;
//         argv[1] = receiver;
//         argv[2] = method_id;
//     }
 
//     for( argi = 0; argi < (argc + __FUNCALL_ARGC); argi++ ) {
//         rb_gc_register_address(&argv[argi]);
//     }
 
//     result = rb_rescue2(___method, (VALUE)argv,
//                         RUBY_METHOD_FUNC(__rescue), (VALUE)argv,
//                         rb_eException, (VALUE) 0);
 
//     for( argi = 0; argi < (argc + __FUNCALL_ARGC); argi++ ) {
//         rb_gc_unregister_address(&argv[argi]);
//     }
 
//     return( result );
// }

// VALUE __funcall_ex(VALUE data) {
//     VALUE *args = (VALUE*) data;
 
//     if( args[0] > 0 ) {
//         return rb_funcall2(args[1], args[2], args[0], &args[3]);
//     } else {
//         return rb_funcall(args[1], args[2], args[0]);
//     }
// }


// VALUE __new_rb_obj(const char *name,int ac, VALUE av[]) {
//  ID class_id = rb_intern(name);
//  // VALUE klass = rb_rescue2(RUBY_METHOD_FUNC(rb_const_get), class_id,
//  //                        RUBY_METHOD_FUNC(__rescue), NULL,
//  //                        rb_eException, (VALUE) 0);
//  // VALUE klass = __funcall(RUBY_METHOD_FUNC(rb_const_get),rb_cObject,class_id,1);

//  VALUE klass = rb_const_get(rb_cObject, class_id);
//  if (NIL_P(klass))
//      SAYX(EXIT_FAILURE,"%s not found",name);
//  // VALUE obj = __funcall(RUBY_METHOD_FUNC(rb_class_new_instance),ac + 1,av,klass,3);
//  VALUE obj = rb_class_new_instance(ac, av, klass);
//  return obj;
// }
        // work
        // @app.call(@request.env)

        // VALUE av[1] = { rb_str_new(p->payload, p->len) };
        // VALUE io = __new_rb_obj("StringIO",1,av);
        // VALUE env = rb_hash_new();
        // // rb_io_write(io, rb_str_new(c->input.bytes, c->input.len));
        // rb_hash_aset(env,rb_str_new2("rack.input"),io);
        // rb_hash_aset(env,rb_str_new2("REQUEST_METHOD"),rb_str_new2("GET"));
        // rb_hash_aset(env,rb_str_new2("CONTENT_TYPE"),rb_str_new2("application/octet-stream"));
        // // rb_hash_aset(env,rb_str_new2("REQUEST_"),rb_str_new2("/"));
        // rb_hash_aset(env,rb_str_new2("REQUEST_URI"),rb_str_new2("/"));
        // VALUE app = __new_rb_obj("Application",0,NULL);
        // VALUE response = rb_funcall(app,rb_intern("call"),1,env,0);
        // VALUE xx = rb_funcall(response,rb_intern("inspect"),0);
        // d_reset(&p);
        // d_append(&p,RSTRING_PTR(xx),RSTRING_LEN(xx));







    // char * file = "main.rb";
    // int fake_argc = 2;
    // char *fake_args[2] = { file, file };
    // ruby_sysinit(&fake_argc, (char ***) &fake_args);
    // RUBY_INIT_STACK;
    // ruby_init();
    // ruby_init_loadpath();

    // void* node = ruby_options(fake_argc, fake_args);
    // ruby_run_node(node);


    // void* node = rb_load_file(file);
    // rb_load_protect(rb_str_new2("./main.rb"), 0, &status);
    // if (status) {
    //  VALUE rbError = rb_funcall(rb_gv_get("$!"), rb_intern("message"), 0);
    //  SAYX(EXIT_FAILURE,"%s",StringValuePtr(rbError));
    // }