#ifndef RRNR_H
#define RRNR_H

#include <Rdefines.h>

#define RRNR_BUILTIN 1
#define RRNR_DOTCALL 2

#define HANDLERS_CNT 11

void RRnR_register_handlers(int(*before_handler)(SEXP, SEXP, SEXP, SEXP, int), SEXP(*after_handler)(SEXP, SEXP, SEXP, SEXP, int, SEXP));
void RRnR_register_eval_handlers(void(*before_handler)(SEXP, SEXP), void(*after_handler)(SEXP, SEXP));
void RRnR_register_JIT_handlers(int(*before_handler)(SEXP), void(*after_handler)(SEXP));
void RRnR_register_stdout_vfprintf_handler(void(*handler)(int, const char *, va_list));
void RRnR_register_browser_handlers(int(*before_handler)(), void(*after_handler)());
void RRnR_register_error_handler(void(*handler)(const char *));
void RRnR_register_trace_state_handler(void(*handler)(Rboolean));

void RRnR_get_all_handlers(void ** arr);
void RRnR_remove_all_handlers(void ** backup);
void RRnR_restore_all_handlers(void ** backup);

int RRnR_get_R_OutputCon();

#endif // RRNR_H
