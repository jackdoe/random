//- adding _GNU_SOURCE makes it work with less than 1_000_000 iterations
// because the malloc in canonicalize_file_name has address > 2^32
// and without _GNU_SOURCE the function is not declared so return value is assumed INT
// and basically is mangling the address

//#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
void OS_get_table()
{
    char * link = (char *) canonicalize_file_name("/tmp/__1234");
//    char * link = (char *) realpath("/tmp",NULL);
    if (link)
        printf("%s\n",link);
}
