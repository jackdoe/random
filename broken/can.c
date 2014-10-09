//- adding _GNU_SOURCE makes it work with less than 1_000_000 iterations
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
