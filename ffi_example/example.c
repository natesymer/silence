#include "example.h"
#include <stdio.h>
#include <stdlib.h>

void * testffi(void *argv,int argc) {
  printf("printed from the FFI!\n");
  
  // return '()
  int *ret = malloc(1*sizeof(int));
  ret[0] = 4;
  return ret;
}