#include "Expression.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// MEMORY MANAGEMENT

// allocate a general expression
Expression * mallocExpr(uint8_t tc,void *mem) {
  Expression *e = (Expression *)malloc(sizeof(Expression));
  e->typecode = tc;
  e->memory = mem;
  return e;
}

// free any expression
Expression * copyExpression(Expression *e) {
  Expression *cpy = NULL;
  if (e) {
    cpy = mallocExpr(e->typecode,NULL);
  
    if (e->memory) {
      if (isCell(e)) {
        GET_STRUCT(e,Cell,cell);
        ALLOC_STRUCT(Cell,new);
        new->car = cell->car ? copyExpression(cell->car) : NULL;
        new->cdr = cell->cdr ? copyExpression(cell->cdr) : NULL;
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
      } else if (isAtom(e)) {
        GET_STRUCT(e,Atom,a);
        ALLOC_STRUCT(Atom,new);
        new->buf = (char *)malloc(sizeof(char)*(a->len));
        new->len = a->len;
        cpy->memory = new;
        memcpy(new->buf,a->buf,a->len);
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
        freeExpression(car(e));
        freeExpression(cdr(e));
      } else if (isAtom(e)) {
        GET_STRUCT(e,Atom,a);
        free(a->buf);
      }
      free(e->memory);
    }
    free(e);
  }
}

// Predicates

// always return true for any expression
int always(Expression *e) {return 1;}
int never(Expression *e) {return 0;}

// ATOM

Expression * mkAtom(char *str,int len) {
  ALLOC_STRUCT(Atom,a);
  a->len = len;
  a->buf = (char *)malloc(sizeof(char)*len);
  strncpy(a->buf,str,len);
  return mallocExpr(0,a);
}

int isAtom(Expression *e) { return e->typecode == 0; }

// NUMBER

Expression * mkNumber(int64_t num,int64_t den) {
  ALLOC_STRUCT(Number,n);
  n->numerator = num;
  n->denominator = den;
  return mallocExpr(1,n);
}

int isNumber(Expression *e) {return e->typecode == 1;}
int64_t numerator(Expression *e) { GET_STRUCT(e,Number,n); return n->numerator; }
int64_t denominator(Expression *e) { GET_STRUCT(e,Number,n); return n->numerator; }

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
  ALLOC_STRUCT(Procedure,p);
  p->evalArgs = evalArgs;
  p->arity = arity;
  p->body = body;
  return mallocExpr(4,p);
}

int isProcedure(Expression *e) { return e->typecode == 4; }

// CELL

Expression * mkCell(Expression *a,Expression *d) {
  ALLOC_STRUCT(Cell,c);
  c->car = a;
  c->cdr = d;
  return mallocExpr(6,c);
}

int isCell(Expression *e) { return e->typecode == 6; }
Expression * car(Expression *e) { GET_STRUCT(e,Cell,c); return c->car; }
Expression * cdr(Expression *e) { GET_STRUCT(e,Cell,c); return c->cdr; }

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
  Expression *res = NULL;
  for (int i = 0; i < len; i++) {
    char ic = in[i];
    Expression *new = snoc(res,mkNumber((int64_t)ic,1));
    freeExpression(res);
    res = new;
  }
  return res;
}

Expression * snoc(Expression *lst,Expression *v) {
  Expression *newcdr = mkCell(copyExpression(v),NULL);
  if (!lst) return newcdr;
  else {
    Expression *cpy = copyExpression(lst);
    Expression *last = cpy;
    while (last && isCell(last) && cdr(last) && isCell(cdr(last))) last = cdr(last);
    
    GET_STRUCT(last,Cell,ptr);
    freeExpression(ptr->cdr); // doesn't null expression pointer
    ptr->cdr = newcdr;
    return cpy;
  }
}

// POINTER

Expression * mkPointer(void *ptr,PtrFinalizer f) {
  ALLOC_STRUCT(Pointer, p);
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