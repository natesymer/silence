#include "Expression.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// MEMORY MANAGEMENT

// general expression allocation. Takes a "typecode"
// and a memory pointer.
Expression * mallocExpr(uint8_t tc,void *mem) {
  Expression *e = ALLOC(Expression);
  e->typecode = tc;
  e->memory = mem;
  return e;
}

// copy any expression
Expression * copyExpression(Expression *e) {
  Expression *cpy = NULL;
  if (e) {
    cpy = mallocExpr(e->typecode,NULL);
  
    if (e->memory) {
      if (isCell(e)) {
        Cell *old = MEMORY(e,Cell);
        Cell *new = ALLOC(Cell);
        new->car = copyExpression(old->car);
        new->cdr = copyExpression(old->cdr);
        cpy->memory = new;
      } else if (isAtom(e)) {
        Atom *old = MEMORY(e,Atom);
        Atom *new = ALLOC(Atom);
        new->buf = (char *)malloc(sizeof(char)*(old->len));
        new->len = old->len;
        cpy->memory = new;
        memcpy(new->buf,old->buf,old->len);
      } else if (isNumber(e)) {
        cpy->memory = ALLOC(Number);
        memcpy(cpy->memory,e->memory,sizeof(Number));
      } else if (isPointer(e)) {
        cpy->memory = ALLOC(Pointer);
        memcpy(cpy->memory,e->memory,sizeof(Pointer));
      } else if (isProcedure(e)) {
        cpy->memory = ALLOC(Procedure);
        memcpy(cpy->memory,e->memory,sizeof(Procedure));
      } 
    }
  }

  return cpy;
}

// free any expression
void freeExpression(Expression *e) {
  if (e) {
    if (e->memory) {
      if (isCell(e)) {
        freeExpression(MEMORY(e,Cell)->car);
        freeExpression(MEMORY(e,Cell)->cdr);
      } else if (isAtom(e)) {
        free(MEMORY(e,Atom)->buf);
      }
      free(e->memory);
    }
    free(e);
  }
  e = NULL;
}

// Predicates

// always return true for any expression
int always(Expression *e) {return 1;}
int never(Expression *e) {return 0;}

// ATOM

Expression * mkAtom(char *str,int len) {
  Atom *a = ALLOC(Atom);
  a->len = len;
  a->buf = (char *)malloc(sizeof(char)*len);
  strncpy(a->buf,str,len); // TODO: copy & calc length better
  return mallocExpr(0,a);
}

int isAtom(Expression *e) { return e->typecode == 0; }

// NUMBER

Expression * mkNumber(int64_t num,int64_t den) {
  Number *n = ALLOC(Number);
  n->numerator = num;
  n->denominator = den;
  return mallocExpr(1,n);
}

int isNumber(Expression *e) {return e->typecode == 1;}
int64_t numerator(Expression *e) { return MEMORY(e,Number)->numerator; }
int64_t denominator(Expression *e) { return MEMORY(e,Number)->denominator; }

// BOOL

Expression * mkBoolTrue() { return mallocExpr(2,NULL); }
Expression * mkBoolFalse() { return mallocExpr(3,NULL); }
int isBool(Expression *e) { return e->typecode == 2 || e->typecode == 3; }

int isTruthy(Expression *e) {
  if (!isBool(e)) return 1;
  else if (e->typecode == 2) return 1;
  else return 0;
}

// PROCEDURE

// TODO write @apply@? maybe import it?

Expression * mkProcedure(uint8_t evalArgs,int8_t arity, CSig body) {
  Procedure *p = ALLOC(Procedure);
  p->evalArgs = evalArgs;
  p->arity = arity;
  p->body = body;
  return mallocExpr(4,p);
}

int isProcedure(Expression *e) { return e->typecode == 4; }

// CELL

Expression * mkCell(Expression *a,Expression *d) {
  Cell *c = ALLOC(Cell);
  c->car = a;
  c->cdr = d;
  return mallocExpr(6,c);
}

int isCell(Expression *e) { return e->typecode == 6; }
Expression * car(Expression *e) { return MEMORY(e,Cell)->car; }
Expression * cdr(Expression *e) { return MEMORY(e,Cell)->cdr; }

// Get the length of a list. Returns -1 on a non-list.
int listLength(Expression *e, int (*pred)(Expression *)) {
  if (!e) return 0;
  else if (isCell(e)) {
    Expression *ep = cdr(e);
    if (ep && isCell(ep) && (*pred)(car(e))) {
      return 1+listLength(cdr(e),pred);
    }
  }
  return -1;
}

int isList(Expression *e) { return listLength(e,&always) != -1; }

// turn a LISP string (list of numbers) into a char *.
// if *out is a buffer, it is used. Otherwise, a new
// buffer is allocated to accommodate each "character".
// TODO: recursive
int toString(Expression *cell, char **out) {
  if (out == NULL) {
    int len = listLength(cell,&isNumber);
    if (len == -1) return -1;
    else *out = malloc(sizeof(char)*len);
  }
  
  int idx = 0;
  Expression *a, *b, *x;
  x = cell;
  while ((b = cdr(x)) && (a = car(x))) {
    (*out)[idx++] = (char)numerator(a);
    
    if (!b) {
      (*out)[idx++] = '\0';
      return 0;
    } else {
      x = b;
    }
  }
  
  free(*out);
  *out = NULL;
  return -1;
}

Expression * fromString(char *in,int len) {
  if (len == 0) return NULL;
  else return mkCell(mkNumber(*in,1),fromString(in+1,len-1));
}

Expression * snoc(Expression *lst,Expression *v) {
  if (!lst) return mkCell(copyExpression(v),NULL);
  else {
    Expression *cpy = copyExpression(lst);
    Expression *last = cpy;
    while (last && isCell(last) && cdr(last) && isCell(cdr(last))) last = cdr(last);
    
    Cell *mem = MEMORY(last,Cell);
    freeExpression(mem->cdr);
    mem->cdr = snoc(mem->cdr,v);
    return cpy;
  }
}

// POINTER

Expression * mkPointer(void *ptr,PtrFinalizer f) {
  Pointer *p = ALLOC(Pointer);
  p->ptr = ptr;
  p->finalizer = f;
  return mallocExpr(7,p);
}

int isPointer(Expression *e) { return e->typecode == 7; }
void * getPointer(Expression *e) { return MEMORY(e,Pointer)->ptr; }

void finalizePointer(Expression *e) {
  Pointer *p = MEMORY(e,Pointer);
  (*(p->finalizer))(p->ptr);
} 