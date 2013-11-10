#include <string.h>
#include <limits.h>

void normalize(const char *path, char *dest) {
    char *stack[PATH_MAX];
    char *token, *save,*begin;
    bzero(stack,sizeof(stack));
    size_t pos = 0;
    for(begin = path;token = strtok_r(begin, "/", &save); begin = save) {
        if (strcmp(token,"..") == 0) {
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
        memcpy(dest + offset,stack[i],len);
        dest[offset + len] = '/';
        offset += len + 1;
    }
    dest[offset > 0 ? offset - 1 : 0] = '\0';
}

int main(int ac, char **av) {
    char dest[PATH_MAX];
    normalize(av[1],dest);
    printf("%s\n\n",dest);
}
