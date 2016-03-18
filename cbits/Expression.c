#include "Expression.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ALLOC_SINGLETON(t,name,v) t *name = (t *)malloc(sizeof(t)); *name = (v);
#define COPY_SINGLETON(t,dest,src) { ALLOC_SINGLETON(t,tmp,*((t *)(src))); *((t *)dest) = *tmp; }

// allocate a general expression
Expression * mallocExpr(uint8_t tc,void *mem) {
  Expression *e = malloc(sizeof(Expression));
  e->typecode = tc;
  e->memory = mem;
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
  if (isCell(e)) {
    freeExpression(car(e));
    freeExpression(cdr(e));
  } else if (isPointer(e)) {
    finalizePointer(e);
  } else if (isAtom(e)) {
    struct Atom *a = (struct Atom *)(e->memory);
    free(a->buf);
  }
  
  free(e->memory);
  free(e);
}

// ATOM

Expression * mkAtom(char *str,int len) {
  struct Atom *a = (struct Atom *)malloc(sizeof(struct Atom));
  a->len = len;
  a->buf = (char *)malloc(sizeof(char)*len);
  strncpy(a->buf,str,len);
  return mallocExpr(0,a);
}

int isAtom(Expression *e) {
  return e->typecode == 0;
}

// NUMBER

Expression * mkNumber(int64_t num,int64_t den) {
  struct Number *n = (struct Number *)malloc(sizeof(struct Number));
  n->numerator = num;
  n->denominator = den;
  return mallocExpr(1,n);
}

int isNumber(Expression *e) {
  return e->typecode == 1;
}

// BOOL

Expression * mkBoolTrue() {
  return mallocExpr(2,NULL);
}

Expression * mkBoolFalse() {
  return mallocExpr(3,NULL);
}

int isBool(Expression *e) { return e->typecode == 2 || e->typecode == 3; }

int isTruthy(Expression *e) {
  if (!isBool(e)) return 1;
  else if (e->typecode == 2) return 1;
  else return 0;
}

// PROCEDURE

// TODO write @apply@? maybe import it?

Expression * mkProcedure(uint8_t evalArgs,int8_t arity, CSig body) {
  struct Procedure *p = (struct Procedure *)malloc(sizeof(struct Procedure));
  p->evalArgs = evalArgs;
  p->arity = arity;
  p->body = body;
  return mallocExpr(4,p);
}

int isProcedure(Expression *e) { return e->typecode == 4; }

// NULL

Expression * mkNull() {
  return mallocExpr(5,NULL);
}

int isNull(Expression *e) { return e->typecode == 5; }

// CELL

Expression * mkCell(Expression *a,Expression *d) {
  struct Cell *c = (struct Cell *)malloc(sizeof(struct Cell));
  c->car = a;
  c->cdr = d;
  return mallocExpr(6,c);
}

int isCell(Expression *e) { return e->typecode == 6; }

Expression * car(Expression *cell) {
  return (((struct Cell *)(cell->memory))->car);
}

Expression * cdr(Expression *cell) {
  return (((struct Cell *)(cell->memory))->cdr);
}

int listLength(Expression *cell, int (*pred)(Expression *), int *out) {
  if (isNull(cell)) return 0;
  else if (isCell(cell)) {
    Expression *ep = cdr(cell);
    Expression *v = car(cell);
    int res = (*pred)(v);
    if (ep && v && res) {
      (*out)++;
      return listLength(ep,pred,out);
    }
  }
  *out = 0;
  return -1;
}

int toString(Expression *cell, char **out) {
  int len = 0;
  if (listLength(cell,&isNumber,&len) == -1) return -1;
  
  char *buf = malloc(sizeof(char)*len);
  Expression *x = cell;
  int idx = 0;
  
  while (1) {
    Expression *a, *b;
    if ((b = cdr(x)) && (a = car(x))) {
      if (isNull(b)) {
        buf[len] = '\0';
        return 0;
      } else {
        buf[idx++] = (char)a;
        x = b;
      }
    } else {
      return -1;
      break;
    }
  }
}


// POINTER

Expression * mkPointer(void *ptr,PtrFinalizer f) {
  struct Pointer *p = (struct Pointer *)malloc(sizeof(struct Pointer));
  p->ptr = ptr;
  p->finalizer = f;
  return mallocExpr(7,p);
}

int isPointer(Expression *e) { return e->typecode == 7; }

void * getPointer(Expression *e) {
  return ((struct Pointer *)e->memory)->ptr;
}

void finalizePointer(Expression *e) {
  struct Pointer *p = (struct Pointer *)e->memory;
  (*(p->finalizer))(p->ptr);
} 