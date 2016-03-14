#include <stdio.h>
#include <stdlib.h>

typedef struct Expression {
  uint8_t typecode;
  uint8_t num_ptrs;
  void **ptrs;
} Expression;

Expression * mallocExpr(uint8_t tc,uint8_t nptrs);
void freeExpression(Expression *e);

Expression * mkAtom(char *str,int len);
Expression * mkNumber(int64_t num,int64_t den);
Expression * mkNull();

int isNumber(Expression *e);
int numberParts(Expression *e, int64_t *num, int64_t *denom);

int isAtom(Expression *e);
int atomParts(Expression *e, char **ptr, int *len);
int numberAsDouble(Expression *e, double *out);
int numberAsInt(Expression *e, int *out);

Expression * mkBool(uint8_t v);
int isBool(Expression *e);
int isTruthy(Expression *e);