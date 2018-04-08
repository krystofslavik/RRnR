#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R.h>
#include <Rinternals.h>

#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h> 

SEXP do_unlock_env(SEXP env);
SEXP do_get_replay_struct(SEXP expr, SEXP env, SEXP options);
SEXP do_record(SEXP expr, SEXP env, SEXP options);
SEXP do_replay(SEXP replay_struct);

SEXP do_lazyload_before();
SEXP do_lazyload_after();

SEXP do_print_random();
SEXP reset_print_random(SEXP seed);
SEXP get_print_random_counter();
SEXP call_runif(SEXP seed);
SEXP simple_return(SEXP obj);
SEXP test_rprintf_capture();
SEXP C_sleep(SEXP time);
SEXP throw_an_error();
SEXP call_throw_an_error();

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
	CALLDEF(do_unlock_env, 1),
	CALLDEF(do_get_replay_struct, 3),
	CALLDEF(do_record, 3),
	CALLDEF(do_replay, 1),
	
	CALLDEF(do_lazyload_before, 0),
	CALLDEF(do_lazyload_after, 0),
	
	CALLDEF(do_print_random, 0),
	CALLDEF(reset_print_random, 1),
	CALLDEF(get_print_random_counter, 0),
	CALLDEF(call_runif, 1),
	CALLDEF(simple_return, 1),
	CALLDEF(test_rprintf_capture, 0),
	CALLDEF(C_sleep, 1),
	CALLDEF(throw_an_error, 0),
	CALLDEF(call_throw_an_error, 0),
	
	{NULL, NULL, 0}
};

void attribute_visible
R_init_RRnR(DllInfo *dll)
{
	R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
	R_forceSymbols(dll, TRUE);
}
