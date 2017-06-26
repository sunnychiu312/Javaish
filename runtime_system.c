#include <runtime_system.h>
#include <stdlib.h>

// char *__javaish_stack[100];
// int __javaish_sp;

__javaish_array_t __javaish_new_array( size_t sz )
{
    __javaish_array_t arr = (__javaish_array_t)malloc( sizeof( arr[0] ) );
    arr->length = sz;
    arr->items = (int *)malloc( sz * sizeof( arr->items[ 0 ] ) );
    size_t i;
    for( i = 0; i < sz; ++i )
        arr->items[ i ] = 0;
    return arr;
}

void __javaish_main( char **  );

int main( int argc, char ** argv )
{
    __javaish_main( 0 );
}
