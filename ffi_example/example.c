#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../cbits/Expression.h"

Expression * showtype(int argc,Expression **argv);
Expression * print_number(int argc,Expression **argv);
const char * showTypecode(uint8_t typecode);
Expression * get_strlen(int argc,Expression **argv);
Expression * snoc_test(int argc,Expression **argv);
Expression * from_str_test(int arc,Expression **argv);

Expression * showtype(int argc,Expression **argv) {
  const char *tc = showTypecode(argv[0]->typecode);
  return mkAtom((char *)tc,strlen((char *)tc));
}

Expression * print_number(int argc,Expression **argv) {
  struct Number *n = ((struct Number *)(argv[0]->memory));
  printf("%lld/%lld\n",n->numerator,n->denominator);
  return NULL;
}

Expression * get_strlen(int argc,Expression **argv) {
  int len = listLength(argv[0],&isNumber);
  if (len != -1) {
    return mkNumber((int64_t)len,1);
  } else {
    return mkBoolFalse();
  }
}

Expression * snoc_test(int argc,Expression **argv) {
  return snoc(argv[0],argv[1]);
}

Expression * from_str_test(int arc,Expression **argv) {
  return fromString("test_string",11);
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