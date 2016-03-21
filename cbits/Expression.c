#include "Expression.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// allocate a general expression
Expression * mallocExpr(uint8_t tc,void *mem) {
  Expression *e = malloc(sizeof(Expression));
  e->typecode = tc;
  e->memory = mem;
  return e;
}

#define ALLOC_STRUCT(t,name) struct t *name = (struct t *)malloc(sizeof(struct t));

// free any expression
Expression * copyExpression(Expression *e) {
  Expression *cpy = mallocExpr(e->typecode,NULL);

  if (isCell(e)) {
    GET_STRUCT(e,Cell,cell);
    ALLOC_STRUCT(Cell,new);
    new->car = copyExpression(cell->car);
    new->cdr = copyExpression(cell->cdr);
    cpy->memory = new;
  } else if (isNumber(e)) {
    ALLOC_STRUCT(Number,new);
    memcpy(new,e->memory,sizeof(struct Number));
    cpy->memory = new;
  } else if (isPointer(e)) {
    ALLOC_STRUCT(Pointer,new);
    memcpy(new,e->memory,sizeof(struct Pointer));
    cpy->memory = new;
  } else if (isProcedure(e)) {
    ALLOC_STRUCT(Procedure,new);
    memcpy(new,e->memory,sizeof(struct Procedure));
    cpy->memory = new;
  } else if (isNull(e)) {
    cpy->memory = NULL;
  } else if (isAtom(e)) {
    GET_STRUCT(e,Atom,a);
    ALLOC_STRUCT(Atom,new);
    new->buf = (char *)malloc(sizeof(char)*(a->len));
    new->len = a->len;
    cpy->memory = new;
    memcpy(new->buf,a->buf,a->len);
  }
  
  return cpy;
}

// free any expression
void freeExpression(Expression *e) {
  if (isCell(e)) {
    freeExpression(car(e));
    freeExpression(cdr(e));
  } else if (isAtom(e)) {
    struct Atom *a = (struct Atom *)(e->memory);
    free(a->buf);
  }
  
  if (e->memory) free(e->memory);
  if (e) free(e);
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
    if (ep && v && (*pred)(v)) {
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
      int64_t an = ((struct Number *)(a->memory))->numerator;
      buf[idx++] = (char)an;
           
      if (isNull(b)) {
        buf[len] = '\0';
        *out = buf;
        return 0;
      } else {
        x = b;
      }
    } else {
      if (buf) free(buf);
      out = NULL;
      return -1;
      break;
    }
  }
}

Expression * fromString(char *in,int len) {
  Expression *res = mkNull();
  for (int i = 0; i < len; i++) {
    char ic = in[i];
    Expression *new = snoc(res,mkNumber((int64_t)ic,1));
    freeExpression(res);
    res = new;
  }
  return res;
}

Expression * snoc(Expression *lst,Expression *v) {
  Expression *newcdr = mkCell(copyExpression(v),mkNull());
  
  if (isNull(lst)) return newcdr;
  
  Expression *cpy = copyExpression(lst);
  Expression *last = cpy;
  while (isCell(last) && isCell(cdr(last))) last = cdr(last);

  GET_STRUCT(last,Cell,ptr);
  freeExpression(ptr->cdr); // doesn't null expression pointer
  ptr->cdr = newcdr;
  return cpy;
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
  GET_STRUCT(e,Pointer,p);
  printf("before finalize\n");
  (*(p->finalizer))(p->ptr);
  printf("after finalize\n");
} 