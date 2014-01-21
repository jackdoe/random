#include "obj.h"
#include "sereal/sereal.h"
#include "salt.h"

int main(void) {
    obj_init_primitive();
    s_init_writers();
    srand(time(NULL));
    server("0.0.0.0",9000,1);
    obj_destroy_primitive();
    return(0);
}
