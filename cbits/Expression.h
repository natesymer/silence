#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <stdio.h>
#include <stdlib.h>

#define GET_STRUCT(expr,type,name) struct type *name = (struct type *)((expr)->memory);

typedef struct Expression {
  uint8_t typecode;
  void *memory;
} Expression;

typedef Expression * (*CSig)(int,Expression **);
typedef void (*PtrFinalizer)(void *);

struct Atom {
  char *buf;
  int len;
};

struct Number {
  int64_t numerator;
  int64_t denominator;
};

struct Procedure {
  uint8_t evalArgs;
  int8_t arity;
  CSig body;
};

struct Cell {
  Expression *car;
  Expression *cdr;
};

struct Pointer {
  void *ptr;
  PtrFinalizer finalizer;
};

Expression * mallocExpr(uint8_t tc,void *mem);
void freeExpression(Expression *e);
Expression * copyExpression(Expression *e);

Expression * mkAtom(char *str,int len);
int isAtom(Expression *e);
int atomParts(Expression *e, char **ptr, int *len);

Expression * mkNumber(int64_t num,int64_t den);
int isNumber(Expression *e);
int numberParts(Expression *e, int64_t *num, int64_t *denom);

int numberAsDouble(Expression *e, double *out);
int numberAsInt(Expression *e, int *out);

Expression * mkBoolTrue();
Expression * mkBoolFalse();
int isBool(Expression *e);
int isTruthy(Expression *e);

Expression * mkProcedure(uint8_t evalArgs,int8_t arity, CSig body);
int isProcedure(Expression *e);

Expression * mkNull();
Expression * mkCell(Expression *a, Expression *d);
int isNull(Expression *e);
int isCell(Expression *e);
Expression * car(Expression *cell);
Expression * cdr(Expression *cell);

Expression * snoc(Expression *lst,Expression *i);

int listLength(Expression *cell, int (*pred)(Expression *), int *out);
int toString(Expression *cell, char **out);
Expression * fromString(char *in,int len);

Expression * mkPointer(void *ptr,PtrFinalizer f);
int isPointer(Expression *e);
void * getPointer(Expression *e);
void finalizePointer(Expression *e);

#endif