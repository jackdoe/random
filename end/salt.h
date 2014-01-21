#ifndef __SALT_H
#define __SALT_H
#define MESSAGE_COMPLETE 4
#define COOKIE 8
#include "http-parser/http_parser.h"
#include "obj.h"
#include "session.h"

struct client {
    int fd;
    int efd;
    http_parser http_parser;
    struct http_parser_url purl;
    char flags;
    obj input;
    obj url;
    obj output;
    struct session session;
};
#include "view.h"
void server(char *ip, uint16_t port, int n_servers);
#endif
