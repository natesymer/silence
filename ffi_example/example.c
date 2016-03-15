#include "example.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

const char * showTypecode(uint8_t typecode);

Expression * showtype(int argc,Expression **argv) {
  const char *tc = showTypecode(argv[0]->typecode);
  return mkAtom((char *)tc,strlen((char *)tc));
}

Expression * print_number(int argc,Expression **argv) {
  int64_t num = 0;
  int64_t denom = 0;
  numberParts(argv[0],&num,&denom);
  printf("%lld/%lld\n",num,denom);
  return mkNull();
}

Expression * uppercase_atom(int argc,Expression **argv) {
  const char *atom = "UPPERCASE_ATOM";
  return mkAtom((char *)atom,strlen((char *)atom));
}

const char * showTypecode(uint8_t typecode) {
  switch (typecode) {
    case 0: return "atom"; break;
    case 1: return "number"; break;
    case 2: return "bool"; break;
    case 3: return "procedure"; break;
    case 4: return "null"; break;
    case 5: return "cell"; break;
    case 6: return "pointer"; break;
    default: return "invalid type";
  }
}