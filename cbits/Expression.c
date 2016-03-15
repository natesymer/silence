#include "Expression.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ALLOC_SINGLETON(t,name,v) t *name = (t *)malloc(sizeof(t)); *name = (v);
#define COPY_SINGLETON(t,dest,src) { ALLOC_SINGLETON(t,tmp,*((t *)(src))); *((t *)dest) = *tmp; }

// allocate a general expression
Expression * mallocExpr(uint8_t tc,uint8_t nptrs) {
  Expression *e = malloc(sizeof(Expression));
  e->typecode = tc;
  e->num_ptrs = nptrs;
  e->ptrs = (void **)malloc(sizeof(void *)*nptrs);
  return e;
}

// Expression * copyExpr(Expression *e) {
//   Expression *cpy = mallocExpr(e->typecode,e->num_ptrs);
//
//   switch (e->typecode) {
//     case 0: {
//       int len = *((int *)e->ptrs[1]);
//       COPY_SINGLETON(int,cpy->ptrs[1],e->ptrs[1]);
//       cpy->ptrs[0] = (char *)malloc(sizeof(char)*len);
//       strncpy((char *)(cpy->ptrs[0]),(char *)(e->ptrs[0]),len);
//       break;
//     }
//     case 1: {
//       COPY_SINGLETON(int64_t,cpy->ptrs[0],e->ptrs[0]);
//       COPY_SINGLETON(int64_t,cpy->ptrs[1],e->ptrs[1]);
//       break;
//     }
//     case 2: {
//       COPY_SINGLETON(uint8_t,cpy->ptrs[0],e->ptrs[0]);
//       break;
//     }
//     case 3: {
//       COPY_SINGLETON(uint8_t,cpy->ptrs[0],e->ptrs[0]);
//       COPY_SINGLETON(uint8_t,cpy->ptrs[1],e->ptrs[1]);
//       COPY_SINGLETON(CSig,cpy->ptrs[2],e->ptrs[2]);
//       break;
//     }
//     case 5: {
//       COPY_SINGLETON(Expression *,cpy->ptrs[0],e->ptrs[0]);
//       COPY_SINGLETON(Expression *,cpy->ptrs[1],e->ptrs[1]);
//       break;
//     }
//     case 6: {
//       COPY_SINGLETON(void *,cpy->ptrs[0],e->ptrs[0]);
//       break;
//     }
//   }
//
//   return cpy;
// }

// free any expression
void freeExpression(Expression *e) {
  for (uint8_t i = 0; i < e->num_ptrs; i++) free(e->ptrs[i]);
  free(e->ptrs);
  free(e);
  e = NULL;
}

// ATOM

Expression * mkAtom(char *str,int len) {
  char *buffer = (char *)malloc(sizeof(char)*len);
  strncpy(buffer,str,len);
  
  ALLOC_SINGLETON(int,lenbuf,len);

  Expression *atom = mallocExpr(0,2);
  atom->ptrs[0] = buffer;
  atom->ptrs[1] = lenbuf;
  return atom;
}

int isAtom(Expression *e) {
  return e->typecode == 0;
}

int atomParts(Expression *e, char **ptr, int *len) {
  if (!isAtom(e)) return -1;
  else {
    ptr = e->ptrs[0];
    len = e->ptrs[1];
    return 0;
  }
}

// NUMBER

Expression * mkNumber(int64_t num,int64_t den) {
  ALLOC_SINGLETON(int64_t,numbuf,num);
  ALLOC_SINGLETON(int64_t,denbuf,den);
  Expression *e = mallocExpr(1,2);
  e->ptrs[0] = numbuf;
  e->ptrs[1] = denbuf;
  return e;
}

int isNumber(Expression *e) {
  return e->typecode == 1;
}

int numberParts(Expression *e, int64_t *nump, int64_t *denomp) {
  if (!isNumber(e)) return -1;
  else {
    *nump = *((int64_t *)e->ptrs[0]);
    *denomp = *((int64_t *)e->ptrs[1]);
    return 0;
  } 
}

int numberAsDouble(Expression *e, double *out) {
  int64_t n,d;
  if (!numberParts(e,&n,&d)) return -1;
  else {
    *out = n/d;
    return 0;
  }
}

// returns -1 if not an integer, 0 if an integer
int numberAsInt(Expression *e, int *out) {
  int64_t n,d;
  if (!numberParts(e,&n,&d)) return -1;
  else {
    if (d != 1) return -1;
    else {
      *out = n;
      return 0;
    }
  }
}

// BOOL

Expression * mkBool(uint8_t v) {
  ALLOC_SINGLETON(uint8_t,boolbuf,v);
  Expression *e = mallocExpr(2,1);
  e->ptrs[0] = boolbuf;
  return e;
}

int isBool(Expression *e) {
  return e->typecode == 2;
}

int isTruthy(Expression *e) {
  if (!isBool(e)) return 1;
  else if ((*((uint8_t *)(e->ptrs[0]))) != 0) return 1;
  else return 0;
}

// PROCEDURE

Expression * mkProcedure(uint8_t evalArgs,uint8_t arity, CSig body) {
  ALLOC_SINGLETON(uint8_t,eabuf,evalArgs);
  ALLOC_SINGLETON(uint8_t,abuf,arity);
  ALLOC_SINGLETON(CSig,bbuf,body);
  Expression *e = mallocExpr(3,3);
  e->ptrs[0] = eabuf;
  e->ptrs[1] = abuf;
  e->ptrs[2] = bbuf;
  return e;
}

// TODO write @apply@
int isProcedure(Expression *e) {
  return e->typecode == 3;
}

// NULL

Expression * mkNull() {
  return mallocExpr(4,0);
}

int isNull(Expression *e) {
  return e->typecode == 4;
}

// CELL

Expression * mkCell(Expression *a,Expression *d) {
  ALLOC_SINGLETON(Expression *,carbuf,a);
  ALLOC_SINGLETON(Expression *,cdrbuf,d);
  Expression *e = mallocExpr(5,2);
  e->ptrs[0] = carbuf;
  e->ptrs[1] = cdrbuf;
  return e;
}

int isCell(Expression *e) {
  return e->typecode == 5;
}

int car(Expression *cell,Expression **out) {
  if (!isCell(cell)) return -1;
  else {
    *out = *((Expression **)(cell->ptrs[0]));
    return 0;
  }
}

int cdr(Expression *cell,Expression **out) {
  if (!isCell(cell)) return -1;
  else {
    *out = *((Expression **)(cell->ptrs[1]));
    return 0;
  }
}

// POINTER

Expression * mkPointer(void *ptr) {
  ALLOC_SINGLETON(void *,ptrbuf,ptr);
  
  Expression *e = mallocExpr(6,1);
  e->ptrs[0] = ptrbuf;
  return e;
}

int isPointer(Expression *e) {
  return e->typecode == 6;
}

int getPointer(Expression *e,void **out) {
  if (!isPointer(e)) return -1;
  else {
    *out = *((void **)(e->ptrs[0]));
    return 0;
  }
}