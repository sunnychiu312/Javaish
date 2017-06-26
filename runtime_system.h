#include <stddef.h>

// extern char *__javaish_stack[100];
// extern int __javaish_sp;

struct __javaish_array_s {
    size_t length;
    int *items;
};

typedef struct __javaish_array_s *__javaish_array_t;

__javaish_array_t __javaish_new_array( size_t sz );
