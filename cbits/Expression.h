#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <stdio.h>
#include <stdlib.h>

// TODO:
// typedef remaining structs
// better error messages instead of SEGFAULTS on NULL usage

#define GET_STRUCT(expr,type,name) type *name = (type *)((expr)->memory);
#define ALLOC_STRUCT(t,name) t *name = (t *)malloc(sizeof(t));

typedef struct Expression {
  uint8_t typecode;
  void *memory;
} Expression;

typedef Expression * (*CSig)(int,Expression **);
typedef void (*PtrFinalizer)(void *);

typedef struct Atom {
  char *buf;
  int len;
} Atom;

typedef struct Number {
  int64_t numerator;
  int64_t denominator;
} Number;

typedef struct Procedure {
  uint8_t evalArgs;
  int8_t arity;
  CSig body;
} Procedure;

typedef struct Cell {
  Expression *car;
  Expression *cdr;
} Cell;

typedef struct Pointer {
  void *ptr;
  PtrFinalizer finalizer;
} Pointer;

Expression * mallocExpr(uint8_t tc,void *mem);
void freeExpression(Expression *e);
Expression * copyExpression(Expression *e);

Expression * mkAtom(char *str,int len);
int isAtom(Expression *e);
int atomParts(Expression *e, char **ptr, int *len);

Expression * mkNumber(int64_t num,int64_t den);
int isNumber(Expression *e);

Expression * mkBoolTrue();
Expression * mkBoolFalse();
int isBool(Expression *e);
int isTruthy(Expression *e);

Expression * mkProcedure(uint8_t evalArgs,int8_t arity, CSig body);
int isProcedure(Expression *e);

Expression * mkCell(Expression *a, Expression *d);
int isCell(Expression *e);
Expression * car(Expression *cell);
Expression * cdr(Expression *cell);

Expression * snoc(Expression *lst,Expression *i);

int listLength(Expression *e, int (*pred)(Expression *));
int toString(Expression *cell, char **out);
Expression * fromString(char *in,int len);

Expression * mkPointer(void *ptr,PtrFinalizer f);
int isPointer(Expression *e);
void * getPointer(Expression *e);
void finalizePointer(Expression *e);

#endif