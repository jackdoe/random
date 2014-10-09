#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"


MODULE = broken		PACKAGE = broken		
PROTOTYPES: DISABLE

SV*
table()
     CODE:
     OS_get_table();
     RETVAL = &PL_sv_undef;
     OUTPUT:
     RETVAL

