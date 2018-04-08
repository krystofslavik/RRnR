#include <Rinternals.h>

#include <stdlib.h>
#include <unistd.h>

int print_random_counter = 0;

SEXP do_print_random()
{
	print_random_counter++;
	
	// generate a random number
	int rnd = rand() % 1000000;

	// call the print function in R
	SEXP s, t;
	t = s = PROTECT(allocList(2));
	SET_TYPEOF(s, LANGSXP);
	SETCAR(t, install("print")); t = CDR(t);
	SETCAR(t, ScalarInteger(rnd)); t = CDR(t);
	eval(s, R_GlobalEnv);
	UNPROTECT(1);

	return R_NilValue;
}

SEXP reset_print_random(SEXP seed)
{
	print_random_counter = 0;
	srand(asInteger(seed));
	
	return R_NilValue;
}

SEXP get_print_random_counter()
{
	return ScalarInteger(print_random_counter);
}

SEXP call_runif(SEXP seed)
{
	// call the set.seed function in R
	SEXP s, t;
	t = s = PROTECT(allocList(2));
	SET_TYPEOF(s, LANGSXP);
	SETCAR(t, install("set.seed")); t = CDR(t);
	SETCAR(t, seed); t = CDR(t);
	eval(s, R_GlobalEnv);
	UNPROTECT(1);
	
	// call the runif function in R
	t = s = PROTECT(allocList(2));
	SET_TYPEOF(s, LANGSXP);
	SETCAR(t, install("runif")); t = CDR(t);
	SETCAR(t, ScalarInteger(1)); t = CDR(t);
	SEXP res = PROTECT(eval(s, R_GlobalEnv));
	
	// call the print function in R
	t = s = PROTECT(allocList(2));
	SET_TYPEOF(s, LANGSXP);
	SETCAR(t, install("print")); t = CDR(t);
	SETCAR(t, res); t = CDR(t);
	eval(s, R_GlobalEnv);
	UNPROTECT(3);
	
	return R_NilValue;
}

SEXP simple_return(SEXP obj)
{
	return obj;
}

SEXP test_rprintf_capture()
{
	Rprintf("printing using Rprintf\n");
	
	// test callback
	SEXP s, t;
	t = s = PROTECT(allocList(2));
	SET_TYPEOF(s, LANGSXP);
	SETCAR(t, install("print")); t = CDR(t);
	SETCAR(t, mkString("printing using print()")); t = CDR(t);
	eval(s, R_GlobalEnv);
	UNPROTECT(1);
	
	Rprintf("printing using Rprintf again\n");
	
	return R_NilValue;
}

SEXP C_sleep(SEXP time)
{
	sleep(asInteger(time));
	return R_NilValue;
}

SEXP throw_an_error()
{
	error("error");
	return R_NilValue;
}

SEXP call_throw_an_error()
{
	// prepare the function name
	SEXP s, t;
	t = s = PROTECT(allocList(3));
	SET_TYPEOF(s, LANGSXP);
	SETCAR(t, install(":::")); t = CDR(t);
	SETCAR(t, install("RRnR")); t = CDR(t);
	SETCAR(t, install("throw_an_error")); t = CDR(t);
	
	// call the throw_an_error function via a callback
	SEXP s2, t2;
	t2 = s2 = PROTECT(allocList(1));
	SET_TYPEOF(s2, LANGSXP);
	SETCAR(t2, s); t2 = CDR(t2);
	eval(s2, R_GlobalEnv);
	
	UNPROTECT(4);
	
	return R_NilValue;
}
