#include <ctype.h>      /* is_digit */
#include <dirent.h>     /* opendir, readdir_r */
#include <fcntl.h>
#include <stdbool.h>    /* BOOL */
#include <stdio.h>      /* *scanf family */
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/vfs.h>    /* statfs */
#include <errno.h>
/* glibc only goodness */
#include <obstack.h>    /* glibc's handy obstacks */
/* ptheads */
#include <pthread.h>    /* pthead_once */

#define obstack_chunk_alloc     malloc
#define obstack_chunk_free      free
static pthread_once_t   globals_init = PTHREAD_ONCE_INIT;

static long long   			boot_time;
static unsigned				page_size;
static unsigned long long	system_memory;
static unsigned 			system_hertz;

static bool     init_failed = false;


#ifdef	PROCESSTABLE_THREAD
pthread_mutex_t _mutex_table;
pthread_mutex_t _mutex_new;

void
mutex_op(int lock, pthread_mutex_t *mutex)
{
	if (lock == 0) {	/*unlock*/
		pthread_mutex_unlock(mutex);
	} else {		/*lock*/
		pthread_mutex_lock(mutex);
	}
}
#endif

void
mutex_new(int lock)
{
#ifdef	PROCESSTABLE_THREAD
	mutex_op(lock, &_mutex_new);
#endif
}

void
mutex_table(int lock)
{
#ifdef	PROCESSTABLE_THREAD
	mutex_op(lock, &_mutex_table);
#endif
}


bool is_pid(const char* str)
{
    for(; *str; str++) {
        if (!isdigit(*str))
            return false;
    }

    return true;
}

void OS_get_table()
{
#ifdef	PROCESSTABLE_THREAD
	pthread_mutex_init(&_mutex_table, NULL);
	pthread_mutex_init(&_mutex_new, NULL);
#endif
     mutex_table(1);
    DIR             *dir;
    if ((dir = opendir("/proc")) == NULL)
        return;

    struct dirent dir_ent,*dir_result;
    char fake[1024], *link;
    bzero(&dir_ent,sizeof(dir_ent));

    while(readdir_r(dir, &dir_ent, &dir_result) == 0 && dir_result) {
        /* Only look at this file if it's a proc id; that is, all numbers */
        if(!is_pid(dir_result->d_name))
            continue;
        snprintf(fake,1024,"/proc/%s/cwd", dir_result->d_name);
        printf("%s\n",fake);
        link = (char *) canonicalize_file_name(fake);
        printf("%s - %d\n",fake,errno);
        if (link) {
            printf("%s\n",link);
            free(link);
        }
    }
    closedir(dir);
    mutex_table(0);
}


