#include "salt.h"
#include "rdtsc.h"
#include "sereal/proto.h"

static char CHARSET[] = { '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z' };


unsigned long long masks[] = {
    0xAAAAAAAAAAAAAAAAULL,
    0xABABABABABABABABULL,
    0xCCCCCCCCCCCCCCCCULL,
    0x9999999999999999ULL,
    0x9A9A9A9A9A9A9A9AULL,
    0x5555555555555555ULL,
    0xa5a5a5a5a5a5a5a5ULL,
    0x5a5a5a5a5a5a5a5aULL,
};
static u8 racy = 0;

void generate_sid(struct session *s) {
    unsigned long long n = ((rdtsc() * (int) getpid()) & masks[racy++ % (sizeof(masks)/sizeof(masks[0]))]) % RAND_MAX;
    int i = 0;
    for (i = 0; i < sizeof(s->sid.bytes); i++) {
        n = rand() ^ n;
        s->sid.bytes[i] = CHARSET[n % sizeof(CHARSET)];
    }
}

void load(struct session *s) {
    
}

void store(struct session *s) {

}


