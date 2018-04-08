#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Rdefines.h>
#include <R_ext/Print.h>
#include <RRnR.h>
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Rconnections.h>
#include <Rinternals.h>

#include "RRnR_local.h"

extern Rboolean R_Visible;

#define DEBUG 0
#define DEBUG_PROCESSED 0
FILE * f_processed = NULL;

SEXP trace;
int trace_sz;
int trace_cnt;
int trace_pos;

SEXP env_replace_table_keys;
SEXP env_replace_table_values;
int env_replace_table_sz;
int env_replace_table_cnt;

int record_orig_std_out, replay_orig_std_out;
int record_before_invoke_count, record_before_invoke_internal_count;

RRnR_Options RRnR_options;

/* Workaround for nonexisting function. */
#define FRAME_LOCK_MASK (1<<14)
SEXP do_unlock_env(SEXP env)
{
	SET_ENVFLAGS(env, ENVFLAGS(env) & (~ FRAME_LOCK_MASK));
	return R_NilValue;
}

void add_to_trace(SEXP val)
{
	// resize the vector if needed
	if(trace_cnt >= trace_sz)
	{
		SEXP tmp;
		trace_sz *= 2;
		R_PreserveObject(tmp = lengthgets(trace, trace_sz));
		R_ReleaseObject(trace);
		trace = tmp;
	}

	SET_NAMED(val, 2); // enable copy-on-write
	SET_VECTOR_ELT(trace, trace_cnt++, val);
}

SEXP peek_trace()
{
	if(trace_pos < trace_cnt)
		return VECTOR_ELT(trace, trace_pos);
	else
		error("no more recorded calls");

	return R_NilValue;
}

SEXP read_trace()
{
	SEXP ret = peek_trace();
	trace_pos++;
	return ret;
}

/* Adds orig as a key and replacement as a value to the env. replacement table. */
void add_env_replacement(SEXP orig, SEXP replacement)
{
	// if key already exists, just replace the value
	int i = 0;
	for(; i < env_replace_table_cnt; i++)
	{
		if(VECTOR_ELT(env_replace_table_keys, i) == orig)
		{
			SET_VECTOR_ELT(env_replace_table_values, i, replacement);
			return;
		}
	}

	// resize
	if(env_replace_table_cnt >= env_replace_table_sz)
	{
		SEXP tmp;
		env_replace_table_sz *= 2;

		R_PreserveObject(tmp = lengthgets(env_replace_table_keys, trace_sz));
		R_ReleaseObject(env_replace_table_keys);
		env_replace_table_keys = tmp;

		R_PreserveObject(tmp = lengthgets(env_replace_table_values, trace_sz));
		R_ReleaseObject(env_replace_table_values);
		env_replace_table_values = tmp;
	}

	SET_VECTOR_ELT(env_replace_table_keys, env_replace_table_cnt, orig);
	SET_VECTOR_ELT(env_replace_table_values, env_replace_table_cnt, replacement);
	env_replace_table_cnt++;
}

/* Find env as a key in the replacement table and return the corresponding value. */
SEXP replace_env(SEXP env)
{
	int i = 0;
	for(; i < env_replace_table_cnt; i++)
	{
		if(VECTOR_ELT(env_replace_table_keys, i) == env)
			return VECTOR_ELT(env_replace_table_values, i);
	}

	// if not found, return the original
	return env;
}

/*
 * Enabled during C calls, similarly to the callback detection. But this
 * detects error() calls which are stored in the trace and repeated during
 * replay.
 */
void error_handler(const char * message)
{
	SEXP err = PROTECT(mkString(message));
	setAttrib(err, install("_RRnR_error"), PROTECT(ScalarInteger(1)));
	add_to_trace(err);

	UNPROTECT(2);
}

/*
 * Adds a special list value to the trace which contains an expression and environment
 * which should be called during replay. This is useful in situations where C code
 * invoked by .Call etc. calls back into R. We need to repeat the callback but we must
 * not invoke the C code as it may be undeterministic.
 */
void eval_handler_after(SEXP call, SEXP env);
void record_stdout_vfprintf(int con, const char * format, va_list ap);
void eval_handler_before(SEXP call, SEXP env)
{
	RRnR_register_eval_handlers(NULL, eval_handler_after);
	RRnR_register_stdout_vfprintf_handler(NULL);
	RRnR_register_error_handler(NULL);

#if DEBUG
		Rprintf("detected callback: ");
		PrintValue(call);
#endif

	SEXP list = PROTECT(allocVector(VECSXP, 2));
	SET_VECTOR_ELT(list, 0, call);
	SET_VECTOR_ELT(list, 1, env);
	setAttrib(list, install("_RRnR_callback"), PROTECT(ScalarInteger(1)));

	add_to_trace(list);

	UNPROTECT(2);
}

/*
 * The eval_after handler detects when an R call made from C code ends
 * and the control is about to be returned back to the C code. This is
 * the right time to reenable the eval_before handler which detects any
 * other calls back to R.
 */
void eval_handler_after(SEXP call, SEXP env)
{
#if DEBUG
		Rprintf("back from callback: ");
		PrintValue(call);
#endif

	RRnR_register_eval_handlers(eval_handler_before, eval_handler_after);
	RRnR_register_stdout_vfprintf_handler(record_stdout_vfprintf);
	RRnR_register_error_handler(error_handler);
}

/*
 * Record suppressors are used between record_before and record_after of
 * non-.Call calls to temporarily disable recording to avoid recording
 * potential calls made by these calls. The non-.Call calls are not
 * called during replay anyway (except special printing functions) so
 * we must skip any subcalls because they would pollute the trace.
 */
void * old_handlers_before_suppressor[HANDLERS_CNT];
void eval_suppressor_after();
void eval_suppressor_before(SEXP call, SEXP env)
{
	RRnR_remove_all_handlers(old_handlers_before_suppressor);
	RRnR_register_eval_handlers(NULL, eval_suppressor_after);
}

void eval_suppressor_after()
{
	RRnR_restore_all_handlers(old_handlers_before_suppressor);
}

/* ------- MAIN before AND after HANDLERS FOR record AND replay MODES. ------- */

int record_before(SEXP call, SEXP op, SEXP args, SEXP env, int call_type)
{
	int flags = detect_flags(call, op, args, env, call_type);

#if DEBUG_PROCESSED
	const char * func_name = CHAR(PRINTNAME((flags & CAN_PRODUCE_CALLBACK) ? CADR(call) : CAR(call)));
	fprintf(f_processed, "%s\t%s\n", func_name, (flags & CAN_PRODUCE_CALLBACK) ? "external" : (flags & IS_INTERNAL) ? "internal" : "primitive");
#endif

	record_before_invoke_count++;
	if(flags & IS_INTERNAL) record_before_invoke_internal_count++;

	if(flags & SHOULD_BE_HANDLED)
	{
#if DEBUG
		Rprintf("recording call: ");
		PrintValue(call);
#endif

		if(flags & CAN_PRODUCE_CALLBACK)
		{
			RRnR_register_eval_handlers(eval_handler_before, eval_handler_after);
			RRnR_register_stdout_vfprintf_handler(record_stdout_vfprintf);
			RRnR_register_error_handler(error_handler);
		}
		else
			RRnR_register_eval_handlers(eval_suppressor_before, eval_suppressor_after);
	}

	return 1; // proceed with the call
}

SEXP record_after(SEXP call, SEXP op, SEXP args, SEXP env, int call_type, SEXP val)
{
	PROTECT(val);
	int unprotect_cnt = 1;
	int flags = detect_flags(call, op, args, env, call_type);

	if(flags & SHOULD_BE_HANDLED)
	{
		if(flags & CAN_PRODUCE_CALLBACK)
			RRnR_register_error_handler(NULL);

		add_to_trace(val);
		RRnR_register_eval_handlers(NULL, eval_handler_after);
		RRnR_register_stdout_vfprintf_handler(NULL);
	}

	if(flags & IS_ENV_GENERATOR)
	{
#if DEBUG
		Rprintf("recording generator: ");
		PrintValue(call);
#endif

		add_to_trace(val);
	}

	UNPROTECT(unprotect_cnt);

	return val;
}

int replay_before(SEXP call, SEXP op, SEXP args, SEXP env, int call_type)
{
	int flags = detect_flags(call, op, args, env, call_type);

	if(flags & SHOULD_BE_HANDLED)
	{
		/* Instead of opening a real connection we create a fake one with 'textConnection(NULL, "w")'. */
		if(flags & IS_SINK)
		{
			int con = asInteger(CAR(args));

			if(con != -1 && con != 1 && con != record_orig_std_out)
			{
				void * old_handlers_before[HANDLERS_CNT];
				RRnR_remove_all_handlers(old_handlers_before);

				SEXP t, s;
				t = s = PROTECT(allocList(3));
				SET_TYPEOF(s, LANGSXP);
				SETCAR(t, install("textConnection")); t = CDR(t);
				SETCAR(t, R_NilValue); t = CDR(t);
				SETCAR(t, PROTECT(mkString("w"))); t = CDR(t);
				SEXP dummy = PROTECT(eval(s, R_GlobalEnv));

				RRnR_restore_all_handlers(old_handlers_before);

				// substitute the original connection with the dummy one
				SETCAR(args, PROTECT(ScalarInteger(asInteger(dummy))));

				UNPROTECT(4);
			}
			/* We need to substitute the connection with the stdout active before the replaying started. */
			else if(con == record_orig_std_out)
			{
				SETCAR(args, PROTECT(ScalarInteger(replay_orig_std_out)));
				UNPROTECT(1);
			}

			RRnR_register_eval_handlers(eval_suppressor_before, eval_suppressor_after);
			return 1;
		}

		/* Allow write only if it outputs to stdout. */
		if(flags & IS_WRITE)
		{
			int con = asInteger(CADR(args));

			if(con == 1 || con == record_orig_std_out)
			{
				/* We need to substitute the connection with the stdout active before the replaying started. */
				if(con == record_orig_std_out)
				{
					SETCADR(args, PROTECT(ScalarInteger(replay_orig_std_out)));
					UNPROTECT(1);
				}

				RRnR_register_eval_handlers(eval_suppressor_before, eval_suppressor_after);
				return 1;
			}
		}

		/* Instead of opening a real graphic device we open a fake one with 'pdf(NULL)'. */
		if(flags & IS_DEVICE_OPENING)
		{
			void * old_handlers_before[HANDLERS_CNT];
			RRnR_remove_all_handlers(old_handlers_before);

			SEXP t, s;
			t = s = PROTECT(allocList(2));
			SET_TYPEOF(s, LANGSXP);
			SETCAR(t, install("pdf")); t = CDR(t);
			SETCAR(t, R_NilValue); t = CDR(t);
			eval(s, R_GlobalEnv);
			UNPROTECT(1);

			RRnR_restore_all_handlers(old_handlers_before);
		}

		return 0; // suppress the call
	}

	return 1; // proceed with the call
}

/* Replay vfprintf on the right connection. */
void replay_vfprintf(Rconnection con, const char * format, ...)
{
	va_list(ap);

	va_start(ap, format);
	(con->vfprintf)(con, format, ap);
	con->fflush(con);
	va_end(ap);
}

SEXP replay_after(SEXP call, SEXP op, SEXP args, SEXP env, int call_type, SEXP val)
{
	SEXP ret = val;
	PROTECT(val);
	int unprotect_cnt = 1;

	int flags = detect_flags(call, op, args, env, call_type);

	if(flags & SHOULD_BE_HANDLED)
	{
#if DEBUG
		Rprintf("replaying call: ");
		PrintValue(call);
#endif

		for(;;)
		{
			// read next element in the trace
			ret = PROTECT(read_trace());
			unprotect_cnt += 1;

			// it is a callback, we must call it
			if(TYPEOF(ret) == VECSXP && getAttrib(ret, install("_RRnR_callback")) != R_NilValue)
			{
				SEXP call = VECTOR_ELT(ret, 0);
				SEXP env = replace_env(VECTOR_ELT(ret, 1));
#if DEBUG
		Rprintf("replaying callback: ");
		PrintValue(call);
		Rprintf("...in env: ");
		PrintValue(env);
#endif
				eval(call, env);

				continue;
			}

			// if it is an error, we must replay it
			if(TYPEOF(ret) == STRSXP && getAttrib(ret, install("_RRnR_error")) != R_NilValue)
			{
#if DEBUG
		Rprintf("replaying error: ");
		PrintValue(ret);
#endif
				error(CHAR(asChar(ret)));

				continue;
			}

			// if it is a vfprintf, we must replay it
			if(TYPEOF(ret) == STRSXP && getAttrib(ret, install("_RRnR_stdout_vfprintf")) != R_NilValue)
			{
				int con_num = asInteger(getAttrib(ret, install("_RRnR_stdout_vfprintf")));

				/* We need to substitute the connection with the stdout active before the replaying started. */
				if(con_num == record_orig_std_out)
					con_num = replay_orig_std_out;

				Rconnection con = getConnection(con_num);

				replay_vfprintf(con, "%s", CHAR(asChar(ret)));
				continue;
			}

			break;
		}

		RRnR_register_eval_handlers(NULL, eval_handler_after);
	}

	if(flags & IS_ENV_GENERATOR)
	{
#if DEBUG
		Rprintf("replaying generator: ");
		PrintValue(call);
#endif
		// read next element in the trace
		SEXP orig_env = PROTECT(read_trace());
		unprotect_cnt += 1;

		// add key-value to the replacement table
		add_env_replacement(orig_env, ret);
	}

	// if this call returns ENVSXP, try to replace it using the replacement table
	else if(TYPEOF(ret) == ENVSXP)
		ret = replace_env(ret);

	UNPROTECT(unprotect_cnt);

	return ret;
}

/* -------- END OF MAIN EVENT HANDLERS. -------- */

void * old_handlers_before_JIT[HANDLERS_CNT];

void record_JIT_after(SEXP call);
int record_JIT_before(SEXP call)
{
#if DEBUG
		Rprintf("running JIT for: ");
		PrintValue(call);
#endif

	RRnR_remove_all_handlers(old_handlers_before_JIT); // temporarily disable recording
	RRnR_register_JIT_handlers(NULL, record_JIT_after); // but do NOT disable the JIT after handler which will restore the handlers!!!

	return 1;
}

void record_JIT_after(SEXP call)
{
	RRnR_restore_all_handlers(old_handlers_before_JIT); // enable recording again
}

int replay_JIT_before(SEXP call)
{
#if DEBUG
		Rprintf("blocking JIT for: ");
		PrintValue(call);
#endif

	return 0; // disable JIT during replay
}

void replay_JIT_after(SEXP call)
{
}

void * old_handlers_before_lazyload[HANDLERS_CNT];

void do_lazyload_before()
{
#if DEBUG
		Rprintf("lazyload detected\n");
#endif

	RRnR_remove_all_handlers(old_handlers_before_lazyload); // temporarily disable recording/replaying
}

void do_lazyload_after()
{
	RRnR_restore_all_handlers(old_handlers_before_lazyload); // enable recording/replaying again
}

void * old_handlers_before_browser[HANDLERS_CNT];

int record_browser_before()
{
	// do not allow browser to be opened during recording
	return 0;
}

void record_browser_after() {}

void replay_browser_after();
int replay_browser_before()
{
	RRnR_remove_all_handlers(old_handlers_before_browser); // temporarily disable RRnR
	RRnR_register_browser_handlers(NULL, replay_browser_after); // but do NOT disable the browser after handler which will restore the handlers!!!

#if DEBUG
	Rprintf("browser in\n");
#endif

	return 1;
}

void replay_browser_after()
{
#if DEBUG
	Rprintf("browser out\n");
#endif

	RRnR_restore_all_handlers(old_handlers_before_browser); // enable RRnR again
}

void * old_handlers_before_trace_state[HANDLERS_CNT];

void trace_state(Rboolean new_state)
{
	if(new_state == FALSE)
	{
#if DEBUG
	Rprintf("trace state off\n");
#endif
		RRnR_remove_all_handlers(old_handlers_before_trace_state); // temporarily disable RRnR
		RRnR_register_trace_state_handler(trace_state); // but do NOT disable the trace state handler which will restore the handlers!!!
	}
	else
	{
#if DEBUG
	Rprintf("trace state on\n");
#endif
		RRnR_restore_all_handlers(old_handlers_before_trace_state); // enable RRnR again
	}
}

/* Capture C initiated prints, convert format string + arguments to plain text and store it in the trace. */
void record_stdout_vfprintf(int con, const char * format, va_list ap)
{
	if(!RRnR_options.allow_prints) return;
	if(con != 1 && con != record_orig_std_out) return;

	char tmp[1];
	int len = vsnprintf(tmp, 0, format, ap) + 1;
	if(len <= 0) return;

	char * str = (char*)malloc(len);
	int written = vsnprintf(str, len, format, ap);
	str[written] = 0;

	SEXP val = PROTECT(mkString(str));
	setAttrib(val, install("_RRnR_stdout_vfprintf"), PROTECT(ScalarInteger(con)));
	add_to_trace(val);
	UNPROTECT(2);

	free(str);
}

void unregister_all_handlers(void * data)
{
	RRnR_remove_all_handlers(NULL);
}

/* This will be called either from do_record function or from R finally handler in case of an error. */
SEXP do_get_replay_struct(SEXP expr, SEXP env, SEXP options)
{
	int unprotect_cnt = 0;

#if DEBUG_PROCESSED
	fclose(f_processed);
#endif

	// resize trace to the exact size we need
	SEXP new_trace;
	PROTECT(new_trace = lengthgets(trace, trace_cnt));
	unprotect_cnt += 1;

	// release the old trace
	R_ReleaseObject(trace);
	trace_sz = trace_cnt = 0;

	// create the structure to be returned
	SEXP replay_struct, replay_struct_names;
	PROTECT(replay_struct = allocVector(VECSXP, 7));
	PROTECT(replay_struct_names = allocVector(STRSXP, 7));
	unprotect_cnt += 2;

	// create debug list
	SEXP debug, debug_names;
	PROTECT(debug = allocVector(VECSXP, 2));
	PROTECT(debug_names = allocVector(STRSXP, 2));
	unprotect_cnt += 2;

	// fill in debug list
	SET_VECTOR_ELT(debug, 0, PROTECT(ScalarInteger(record_before_invoke_count)));
	SET_STRING_ELT(debug_names, 0, PROTECT(mkChar("invoke_cnt")));
	SET_VECTOR_ELT(debug, 1, PROTECT(ScalarInteger(record_before_invoke_internal_count)));
	SET_STRING_ELT(debug_names, 1, PROTECT(mkChar("invoke_internal_cnt")));
	setAttrib(debug, R_NamesSymbol, debug_names);
	unprotect_cnt += 4;

	// fill in replay_struct
	SET_VECTOR_ELT(replay_struct, 0, expr);
	SET_STRING_ELT(replay_struct_names, 0, PROTECT(mkChar("expression")));
	SET_VECTOR_ELT(replay_struct, 1, R_NilValue);
	SET_STRING_ELT(replay_struct_names, 1, PROTECT(mkChar("result")));
	SET_VECTOR_ELT(replay_struct, 2, new_trace);
	SET_STRING_ELT(replay_struct_names, 2, PROTECT(mkChar("trace")));
	SET_VECTOR_ELT(replay_struct, 3, env);
	SET_STRING_ELT(replay_struct_names, 3, PROTECT(mkChar("environment")));
	SET_VECTOR_ELT(replay_struct, 4, options);
	SET_STRING_ELT(replay_struct_names, 4, PROTECT(mkChar("options")));
	SET_VECTOR_ELT(replay_struct, 5, PROTECT(ScalarInteger(record_orig_std_out)));
	SET_STRING_ELT(replay_struct_names, 5, PROTECT(mkChar("orig_std_out")));
	SET_VECTOR_ELT(replay_struct, 6, debug);
	SET_STRING_ELT(replay_struct_names, 6, PROTECT(mkChar("debug")));
	setAttrib(replay_struct, R_NamesSymbol, replay_struct_names);
	unprotect_cnt += 8;

	UNPROTECT(unprotect_cnt);

	return replay_struct;
}

SEXP getListElement(SEXP list, const char *str)
{
	SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

	for (int i = 0; i < length(list); i++)
		if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
		   elmt = VECTOR_ELT(list, i);
		   break;
		}
	return elmt;
}

void parse_options(SEXP options)
{
	int allow_graphs = asLogical(getListElement(options, "allow_graphs"));
	if(allow_graphs == 0 || allow_graphs == 1) RRnR_options.allow_graphs = allow_graphs;

	int allow_prints = asLogical(getListElement(options, "allow_prints"));
	if(allow_prints == 0 || allow_prints == 1) RRnR_options.allow_prints = allow_prints;

	int allow_connections = asLogical(getListElement(options, "allow_connections"));
	if(allow_connections == 0 || allow_connections == 1) RRnR_options.allow_connections = allow_connections;
}

/* Recording entry point. */
SEXP do_record(SEXP expr, SEXP env, SEXP options)
{
	int unprotect_cnt = 0;

	if(TYPEOF(expr) != EXPRSXP) error("expression vector expected");
	if(LENGTH(expr) != 1) error("exactly one expression expected");
	if(TYPEOF(options) != VECSXP) error("options must be passed as a generic vector");

	parse_options(options);

#if DEBUG_PROCESSED
	f_processed = fopen("processed_calls.txt", "w");
#endif

	// alloc new vector for storing the trace
	trace_sz = 16;
	trace_cnt = 0;
	R_PreserveObject(trace = allocVector(VECSXP, trace_sz));

	// save current stdout connection which will be later used to detect whether to call cat
	record_orig_std_out = RRnR_get_R_OutputCon();

	record_before_invoke_count = 0;
	record_before_invoke_internal_count = 0;

	// run the expression
	RRnR_register_handlers(record_before, record_after);
	RRnR_register_JIT_handlers(record_JIT_before, record_JIT_after);
	RRnR_register_browser_handlers(record_browser_before, record_browser_after);
	RRnR_register_trace_state_handler(trace_state);
		// register error handler which unregisters all RRnR handlers
		RCNTXT cntxt;
		begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
		cntxt.cend = &unregister_all_handlers;

		SEXP result = PROTECT(eval(VECTOR_ELT(expr, 0), env));
		unprotect_cnt += 1;

		endcontext(&cntxt);
	RRnR_remove_all_handlers(NULL);

#if DEBUG
		Rprintf("record_before invoke count: %d\n", record_before_invoke_count);
		Rprintf("record_before invoke internal count: %d\n", record_before_invoke_internal_count);
		Rprintf("trace_cnt: %d\n", trace_cnt);
#endif

	// create the replay struct
	SEXP replay_struct = PROTECT(do_get_replay_struct(expr, env, options));
	unprotect_cnt += 1;

	// insert the result value
	SET_VECTOR_ELT(replay_struct, 1, result);

	UNPROTECT(unprotect_cnt);

	return replay_struct;
}

/* Replaying entry point. */
SEXP do_replay(SEXP replay_struct)
{
	int unprotect_cnt = 0;

	// first read the structure
	if(TYPEOF(replay_struct) != VECSXP) error("replay structure must be a generic vector");
	if(LENGTH(replay_struct) != 7) error("replay structure should have exactly seven elements");

	SEXP expr = VECTOR_ELT(replay_struct, 0);
	if(TYPEOF(expr) != EXPRSXP) error("expression must be an expression vector");
	if(LENGTH(expr) != 1) error("exactly one expression expected");

	SEXP result = VECTOR_ELT(replay_struct, 1);

	// assign trace to the global variable
	trace = VECTOR_ELT(replay_struct, 2);
	if(TYPEOF(trace) != VECSXP) error("trace must be a generic vector");
	trace_sz = trace_cnt = LENGTH(trace);

	SEXP env = VECTOR_ELT(replay_struct, 3);
	if(TYPEOF(env) != ENVSXP) error("environment must be an environment");

	SEXP options = VECTOR_ELT(replay_struct, 4);
	if(TYPEOF(options) != VECSXP) error("options must be a generic vector");

	SEXP record_orig_std_out_sexp = VECTOR_ELT(replay_struct, 5);
	if(TYPEOF(record_orig_std_out_sexp) != INTSXP) error("orig_std_out must be an integer");

	parse_options(options);

	// reset the position
	trace_pos = 0;

	// alloc two vectors for storing the env replace table
	env_replace_table_sz = 16;
	env_replace_table_cnt = 0;
	R_PreserveObject(env_replace_table_keys = allocVector(VECSXP, env_replace_table_sz));
	R_PreserveObject(env_replace_table_values = allocVector(VECSXP, env_replace_table_sz));

	// save current stdout connection which will be later used when calling cat
	replay_orig_std_out = RRnR_get_R_OutputCon();
	record_orig_std_out = asInteger(record_orig_std_out_sexp);

	// run the expression
	RRnR_register_handlers(replay_before, replay_after);
	RRnR_register_JIT_handlers(replay_JIT_before, replay_JIT_after);
	RRnR_register_browser_handlers(replay_browser_before, replay_browser_after);
	RRnR_register_trace_state_handler(trace_state);
		// register error handler which unregisters all RRnR handlers
		RCNTXT cntxt;
		begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
		cntxt.cend = &unregister_all_handlers;

		SEXP new_result = PROTECT(eval(VECTOR_ELT(expr, 0), env));
		unprotect_cnt += 1;

		endcontext(&cntxt);
	RRnR_remove_all_handlers(NULL);

	// release the env replace table
	R_ReleaseObject(env_replace_table_keys);
	R_ReleaseObject(env_replace_table_values);

	// TODO: we can maybe check that the new_result is the same as the original one

	// return the value with visibility flag
	SEXP ret = PROTECT(allocVector(VECSXP, 2));
	SEXP nm = PROTECT(allocVector(STRSXP, 2));
	unprotect_cnt += 2;

	SET_STRING_ELT(nm, 0, PROTECT(mkChar("value")));
	SET_STRING_ELT(nm, 1, PROTECT(mkChar("visible")));
	SET_VECTOR_ELT(ret, 0, new_result);
	SET_VECTOR_ELT(ret, 1, PROTECT(ScalarLogical(R_Visible)));
	setAttrib(ret, R_NamesSymbol, nm);
	unprotect_cnt += 3;

	UNPROTECT(unprotect_cnt);

	return ret;
}
