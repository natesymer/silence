#include <stdio.h>
#include <stdlib.h>

typedef struct Expression {
  uint8_t typecode;
  uint8_t num_ptrs;
  void **ptrs;
} Expression;

typedef Expression * (*CSig)(int,Expression **);

Expression * mallocExpr(uint8_t tc,uint8_t nptrs);
void freeExpression(Expression *e);

Expression * mkAtom(char *str,int len);
int isAtom(Expression *e);
int atomParts(Expression *e, char **ptr, int *len);

Expression * mkNumber(int64_t num,int64_t den);
int isNumber(Expression *e);
int numberParts(Expression *e, int64_t *num, int64_t *denom);

int numberAsDouble(Expression *e, double *out);
int numberAsInt(Expression *e, int *out);

Expression * mkBool(uint8_t v);
int isBool(Expression *e);
int isTruthy(Expression *e);

Expression * mkProcedure(uint8_t evalArgs,uint8_t arity, CSig body);
int isProcedure(Expression *e);

Expression * mkNull();
int isNull(Expression *e);

Expression * mkCell(Expression *a,Expression *d);
int isCell(Expression *e);
int car(Expression *cell,Expression **out);
int cdr(Expression *cell,Expression **out);

Expression * mkPointer(void *ptr);
int isPointer(Expression *e);
int getPointer(Expression *e,void **out);