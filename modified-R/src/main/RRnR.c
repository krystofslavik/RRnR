#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <RRnR.h>

#include <string.h>

int(*RRnR_before_handler)(SEXP, SEXP, SEXP, SEXP, int) = NULL;
SEXP(*RRnR_after_handler)(SEXP, SEXP, SEXP, SEXP, int, SEXP) = NULL;

void RRnR_register_handlers(int(*before_handler)(SEXP, SEXP, SEXP, SEXP, int), SEXP(*after_handler)(SEXP, SEXP, SEXP, SEXP, int, SEXP))
{
	RRnR_before_handler = before_handler;
	RRnR_after_handler = after_handler;
}

int RRnR_before(SEXP call, SEXP op, SEXP args, SEXP rho, int call_type)
{
	return RRnR_before_handler ? RRnR_before_handler(call, op, args, rho, call_type) : 1;
}

SEXP RRnR_after(SEXP call, SEXP op, SEXP args, SEXP rho, int call_type, SEXP val)
{
	return RRnR_after_handler ? RRnR_after_handler(call, op, args, rho, call_type, val) : val;
}




void(*RRnR_eval_before_handler)(SEXP, SEXP) = NULL;
void(*RRnR_eval_after_handler)(SEXP, SEXP) = NULL;

void RRnR_register_eval_handlers(void(*before_handler)(SEXP, SEXP), void(*after_handler)(SEXP, SEXP))
{
	RRnR_eval_before_handler = before_handler;
	RRnR_eval_after_handler = after_handler;
}

int RRnR_eval_before(SEXP call, SEXP env)
{
	if(RRnR_eval_before_handler)
	{
		RRnR_eval_before_handler(call, env);
		return 1;
	}

	return 0;
}

void RRnR_eval_after(SEXP call, SEXP env)
{
	if(RRnR_eval_after_handler) RRnR_eval_after_handler(call, env);
}




int(*RRnR_JIT_before_handler)(SEXP) = NULL;
void(*RRnR_JIT_after_handler)(SEXP) = NULL;

void RRnR_register_JIT_handlers(int(*before_handler)(SEXP), void(*after_handler)(SEXP))
{
	RRnR_JIT_before_handler = before_handler;
	RRnR_JIT_after_handler = after_handler;
}

int RRnR_JIT_before(SEXP call)
{
	return RRnR_JIT_before_handler ? RRnR_JIT_before_handler(call) : 1;
}

void RRnR_JIT_after(SEXP call)
{
	if(RRnR_JIT_after_handler) RRnR_JIT_after_handler(call);
}




void(*RRnR_stdout_vfprintf_handler)(int con, const char * format, va_list ap) = NULL;

void RRnR_register_stdout_vfprintf_handler(void(*handler)(int con, const char * format, va_list ap))
{
	RRnR_stdout_vfprintf_handler = handler;
}

void RRnR_stdout_vfprintf(int con, const char * format, va_list ap)
{
	if(RRnR_stdout_vfprintf_handler) RRnR_stdout_vfprintf_handler(con, format, ap);
}




int(*RRnR_browser_before_handler)() = NULL;
void(*RRnR_browser_after_handler)() = NULL;

void RRnR_register_browser_handlers(int(*before_handler)(), void(*after_handler)())
{
	RRnR_browser_before_handler = before_handler;
	RRnR_browser_after_handler = after_handler;
}

int RRnR_browser_before()
{
	if(RRnR_browser_before_handler)
		return RRnR_browser_before_handler();

	return -1;
}

void RRnR_browser_after()
{
	if(RRnR_browser_after_handler) RRnR_browser_after_handler();
}



void(*RRnR_error_handler)(const char*) = NULL;

void RRnR_register_error_handler(void(*handler)(const char*))
{
	RRnR_error_handler = handler;
}

void RRnR_error(const char * message)
{
	if(RRnR_error_handler) RRnR_error_handler(message);
}




void(*RRnR_trace_state_handler)(Rboolean) = NULL;

void RRnR_register_trace_state_handler(void(*handler)(Rboolean))
{
	RRnR_trace_state_handler = handler;
}

void RRnR_trace_state(Rboolean new_state)
{
	if(RRnR_trace_state_handler) RRnR_trace_state_handler(new_state);
}





void RRnR_get_all_handlers(void ** arr)
{
	arr[0] = RRnR_before_handler;
	arr[1] = RRnR_after_handler;
	arr[2] = RRnR_eval_before_handler;
	arr[3] = RRnR_eval_after_handler;
	arr[4] = RRnR_JIT_before_handler;
	arr[5] = RRnR_JIT_after_handler;
	arr[6] = RRnR_stdout_vfprintf_handler;
	arr[7] = RRnR_browser_before_handler;
	arr[8] = RRnR_browser_after_handler;
	arr[9] = RRnR_error_handler;
	arr[10] = RRnR_trace_state_handler;
}

void RRnR_remove_all_handlers(void ** backup)
{
	if(backup)
		RRnR_get_all_handlers(backup);

	RRnR_register_handlers(NULL, NULL);
	RRnR_register_eval_handlers(NULL, NULL);
	RRnR_register_JIT_handlers(NULL, NULL);
	RRnR_register_stdout_vfprintf_handler(NULL);
	RRnR_register_browser_handlers(NULL, NULL);
	RRnR_register_error_handler(NULL);
	RRnR_register_trace_state_handler(NULL);
}

void RRnR_restore_all_handlers(void ** backup)
{
	RRnR_register_handlers((int(*)(SEXP, SEXP, SEXP, SEXP, int))backup[0], (SEXP(*)(SEXP, SEXP, SEXP, SEXP, int, SEXP))backup[1]);
	RRnR_register_eval_handlers((void(*)(SEXP, SEXP))backup[2], (void(*)(SEXP, SEXP))backup[3]);
	RRnR_register_JIT_handlers((int(*)(SEXP))backup[4], (void(*)(SEXP))backup[5]);
	RRnR_register_stdout_vfprintf_handler((void(*)(int con, const char * format, va_list ap))backup[6]);
	RRnR_register_browser_handlers((int(*)())backup[7], (void(*)())backup[8]);
	RRnR_register_error_handler((void(*)(const char*))backup[9]);
	RRnR_register_trace_state_handler((void(*)(Rboolean))backup[10]);
}


int RRnR_get_R_OutputCon() { return R_OutputCon; }
