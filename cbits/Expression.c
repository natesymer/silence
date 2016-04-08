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

// copy an expression
Expression * copyExpression(Expression *e) {
  Expression *cpy = NULL;
  if (e) {
    cpy = mallocExpr(e->typecode,NULL);
    if (isCell(e)) {
      Cell *new = ALLOC(Cell);
      new->car = copyExpression(MEMORY(e,Cell)->car);
      new->cdr = copyExpression(MEMORY(e,Cell)->cdr);
      cpy->memory = new;
    } else if (isAtom(e)) {
      Atom *old = MEMORY(e,Atom);
      Atom *new = ALLOC(Atom);
      new->buf = ALLOCN(char,old->len);
      new->len = old->len;
      cpy->memory = new;
      memcpy(new->buf,old->buf,new->len);
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
  return cpy;
}

// free an expression
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
  a->buf = ALLOCN(char,len);
  strncpy(a->buf,str,len); // TODO: copy & calc length better
  return mallocExpr(0,a);
}

int isAtom(Expression *e) { return e->memory && e->typecode == 0; }

// NUMBER

Expression * mkNumber(int64_t num,int64_t den) {
  Number *n = ALLOC(Number);
  n->numerator = num;
  n->denominator = den;
  return mallocExpr(1,n);
}

int isNumber(Expression *e) {return e->typecode == 1;}
int isIntegralNumber(Expression *e) {return isNumber(e) && denominator(e) == 1; }
int64_t numerator(Expression *e) { return MEMORY(e,Number)->numerator; }
int64_t denominator(Expression *e) { return MEMORY(e,Number)->denominator; }

// BOOL

Expression * mkBoolTrue() { return mallocExpr(2,NULL); }
Expression * mkBoolFalse() { return mallocExpr(3,NULL); }
int isBool(Expression *e) { return (e->typecode == 2 || e->typecode == 3) && e->memory == NULL; }

int isTruthy(Expression *e) {
  if (!isBool(e)) return 1;
  if (e->typecode == 2) return 1;
  return 0;
}

// PROCEDURE
// TODO: write @apply@? maybe import it?

Expression * mkProcedure(uint8_t evalArgs,int8_t arity, CSig body) {
  Procedure *p = ALLOC(Procedure);
  p->evalArgs = evalArgs;
  p->arity = arity;
  p->body = body;
  return mallocExpr(4,p);
}

int isProcedure(Expression *e) { return e->memory && e->typecode == 4; }

// CELL

Expression * mkCell(Expression *a,Expression *d) {
  Cell *c = ALLOC(Cell);
  c->car = a;
  c->cdr = d;
  return mallocExpr(6,c);
}

int isCell(Expression *e) { return e->memory && e->typecode == 6; }
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
int isString(Expression *e) { return listLength(e,&isIntegralNumber) != -1; }

// Turn a LISP string (list of numbers) into a char *.
// The buffer is reallocated.
// returns:
// 0 -> success
// -1 -> invalid list
int toString(Expression *cell, char **out) {
  int len = listLength(cell,&isIntegralNumber);
  
  if (len != -1) {
    if ((*out = realloc(out,sizeof(char)*len)) == NULL) return 0;
    
    int idx = 0;
    Expression *x = cell;
    Expression *a, *b;
    while ((b = cdr(x)) && (a = car(x))) {
      (*out)[idx++] = (char)numerator(a);
    
      if (b) x = b;
      else {
        (*out)[idx++] = '\0';
        return 0;
      }
    }
  }

  if (*out) {
    free(*out);
    *out = NULL;
  }
  
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

int isPointer(Expression *e) { return e->memory && e->typecode == 7; }
void * getPointer(Expression *e) { return MEMORY(e,Pointer)->ptr; }

void finalizePointer(Expression *e) {
  Pointer *p = MEMORY(e,Pointer);
  (*(p->finalizer))(p->ptr);
} 