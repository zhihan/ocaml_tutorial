#include <stdio.h>
#include "myvector.h"

void main(int argc, char* argv[])
{
    realVector* v = newVector();
    vector_append(v, 1.0);
    vector_append(v, 2.0);
    printf("v[0]=%f\n", vector_get(v, 0));
    printf("v[0]=%f\n", vector_get(v,1));
    
}
