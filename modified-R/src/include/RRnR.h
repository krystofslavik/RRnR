#ifndef RRNR_H
#define RRNR_H

#include <Rdefines.h>
#include <stdarg.h>

#define RRNR_BUILTIN 1
#define RRNR_DOTCALL 2

int RRnR_before(SEXP call, SEXP op, SEXP args, SEXP env, int call_type);
SEXP RRnR_after(SEXP call, SEXP op, SEXP args, SEXP env, int call_type, SEXP val);
int RRnR_eval_before(SEXP call, SEXP env);
void RRnR_eval_after(SEXP call, SEXP env);
int RRnR_JIT_before(SEXP call);
void RRnR_JIT_after(SEXP call);
void RRnR_stdout_vfprintf(int con, const char * format, va_list ap);
int RRnR_browser_before();
void RRnR_browser_after();
void RRnR_error(const char * message);
void RRnR_trace_state(Rboolean new_state);

#endif // RRNR_H
