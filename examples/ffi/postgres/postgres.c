#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libpq-fe.h>
#include "../../../cbits/Expression.h" // wherever you can find Expression.h

Expression * connect_pg(int argc,Expression **argv);
Expression * query_pg(int argc,Expression **argv);
void free_connection(void *conn);
Expression * pgresult_to_expression(PGresult *res);

void free_connection(void *conn) {
  PQfinish((PGconn *)conn);
}

Expression * connect_pg(int argc,Expression **argv) {
  return mkPointer(PQconnectdb(""),&free_connection);
}

Expression * query_pg(int argc,Expression **argv) {
  if (argc == 2) {
    Expression *connptr = argv[0];
    Expression *query = argv[1];
    
    char *queryStr = NULL;
    toString(query,&queryStr);

    if (isPointer(connptr) && queryStr) {
      PGconn *conn = ((struct Pointer *)(connptr->memory))->ptr;
      PGresult *res = PQexec(conn,queryStr);
      Expression *eres = pgresult_to_expression(res);
      PQclear(res);
      return eres;
    } else {
      return NULL;
    }
  } else {
    return NULL;
  }
}

Expression * pgresult_to_expression(PGresult *res) {
  if (PQresultStatus(res) == PGRES_TUPLES_OK || PQresultStatus(res) == PGRES_SINGLE_TUPLE) {
    Expression *e = NULL;

    for (int r = 0; r < PQntuples(res); r++) {
      Expression *re = NULL;
      for (int c = 0; c < PQnfields(res); c++) {
        Expression *name = mkAtom(PQfname(res,c),strlen(PQfname(res,c)));
        Expression *value = PQgetisnull(res,r,c) ? NULL : fromString(PQgetvalue(res,r,c),strlen(PQgetvalue(res,r,c)));
        re = snoc(re,mkCell(name,value));
      }
      e = snoc(e,re);
    }
    return e;
  } else  if (PQresultStatus(res) == PGRES_COMMAND_OK) {
    return mkBoolTrue();
  } else {
    return mkBoolFalse();
  }
}