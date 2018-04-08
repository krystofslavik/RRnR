#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>

#include <Rdefines.h>
#include <R_ext/Print.h>
#include <RRnR.h>

#include "RRnR_local.h"

/*
 * All built-in functions are defined in names.c. There are flags specifying
 * whether a function is Internal/Primitive and Builtin/Special.
 * Builtin functions get all their arguments always evaluated.
 * Special functions evaluate their arguments themselves.
 * Internal functions get called by a wrapper closure which handles named and default arguments.
 * Primitive function get called directly.
 *
 * Although we could record all of them, it would become really crazy really quickly as the
 * trace would grow insanely large. Therefore we must select only those functions which have
 * some side effect.
 *
 * Another thing to keep in mind is that the bytecode compiler handles some of these built-in
 * functions explicitly. This means that we will not catch these functions here. For example
 * .Call, .Internal, if, assignment operators and many more. List of these functions can be
 * obtained from cmp.R in the compiler package source, search for 'setInlineHandler' calls.
 */

static char * builtins_to_handle[] =
{
	/* Misc */
	"scan",
	"setFileTime",
	"save",
	"load",
	"readline",
	"Sys.time",

	/* System */
	"proc.time",
	"gc.time",
	"Version",
	"machine",
	"commandArgs",
	"internalsID",
	"system",
	"shell.exec",
	"Sys.which",
	"mkjunction",
	"tzone_name",
	"address",
	/*"refcnt",*/
	"capabilities",
	"capabilitiesX11",
	"l10n_info",
	"Cstack_info",
	"eSoftVersion",
	"curlVersion",

	/* Networking */
	"curlGetHeaders",
	"curlDownload",

	/* Random */
	"rchisq",
	"rexp",
	"rgeom",
	"rpois",
	"rt",
	"rsignrank",
	"rbeta",
	"rbinom",
	"rcauchy",
	"rf",
	"rgamma",
	"rlnorm",
	"rlogis",
	"rnbinom",
	"rnbinom_mu",
	"rnchisq",
	"rnorm",
	"runif",
	"rweibull",
	"rwilcox",
	"rhyper",
	"sample",
	"sample2",
	"RNGkind",
	"set.seed",

	/* OS functions */
	"file.show",
	"file.create",
	"file.remove",
	"file.rename",
	"file.append",
	"file.symlink",
	"file.link",
	"file.copy",
	"list.files",
	"list.dirs",
	"file.exists",
	"file.choose",
	"file.info",
	"file.access",
	"dir.exists",
	"dir.create",
	"tempfile",
	"tempdir",
	"R.home",
	"date",
	"Sys.getenv",
	"Sys.setenv",
	"Sys.unsetenv",
	"getwd",
	"setwd",
	"basename",
	"dirname",
	"Sys.chmod",
	"Sys.umask",
	"Sys.readlink",
	"Sys.info",
	"Sys.sleep",
	"Sys.getlocale",
	"Sys.setlocale",
	"Sys.localeconv",
	"path.expand",
	"Sys.getpid",
	"normalizePath",
	"Sys.glob",
	"unlink"
};
static int num_to_handle = sizeof(builtins_to_handle) / sizeof(builtins_to_handle[0]);
static SEXP symbols_to_handle[sizeof(builtins_to_handle) / sizeof(builtins_to_handle[0])];

static char * builtins_to_handle_connections[] =
{
	"stdin",
	"stdout",
	"stderr",
	"isatty",
	"readLines",
	// (print) "writeLines",
	"readBin",
	// (print) "writeBin",
	"readChar",
	// (print) "writeChar",
	"open",
	"isOpen",
	"isIncomplete",
	"isSeekable",
	"close",
	"flush",
	"file",
	"url",
	"pipe",
	"fifo",
	"gzfile",
	"bzfile",
	"xzfile",
	"unz",
	"seek",
	"truncate",
	"pushBack",
	"clearPushBack",
	"pushBackLength",
	"rawConnection",
	"rawConnectionValue",
	"textConnection",
	"textConnectionValue",
	"socketConnection",
	"sockSelect",
	"getConnection",
	"getAllConnections",
	"summary.connection",
	"gzcon",
	"memCompress",
	"memDecompress",

	"saveToConn",
	"loadFromConn2",
	"serializeToConn",
	"unserializeFromConn",
	"parse",
	"serialize",
	"serializeb",
	"unserialize"
};
static int num_to_handle_connections = sizeof(builtins_to_handle_connections) / sizeof(builtins_to_handle_connections[0]);
static SEXP symbols_to_handle_connections[sizeof(builtins_to_handle_connections) / sizeof(builtins_to_handle_connections[0])];

static char * builtins_to_handle_prints[] =
{
	"sink",
	"sink.number",

	"print.default",
	"print.function",
	"prmatrix",

	"cat",
	"writeLines",
	"writeChar",
	"writeBin",
	"dput",
	"dump"
};
static int num_to_handle_prints = sizeof(builtins_to_handle_prints) / sizeof(builtins_to_handle_prints[0]);
static SEXP symbols_to_handle_prints[sizeof(builtins_to_handle_prints) / sizeof(builtins_to_handle_prints[0])];
static int writes_offset = 5;

static char * env_generators[] =
{
	"new.env",
	"environment",
	"parent.frame",
	"as.environment",
	"pos.to.env",
	"sys.frame",
	"parent.env",
	"topenv"
};
static int num_env_generators = sizeof(env_generators) / sizeof(env_generators[0]);
static SEXP symbols_env_generators[sizeof(env_generators) / sizeof(env_generators[0])];

static char * calls[] =
{
	".C",
	".Fortran",
	".External",
	".External2",
	".External.graphics",
	".Call",
	".Call.graphics"
};
static int num_calls = sizeof(calls) / sizeof(calls[0]);
static SEXP symbols_calls[sizeof(calls) / sizeof(calls[0])];

static char * device_opening[] =
{
	"C_Quartz",
	"C_devCairo",
	"C_PDF",
	"C_PostScript",
	"C_XFig",
	"C_PicTeX"
};
static int num_device_opening = sizeof(device_opening) / sizeof(device_opening[0]);
static SEXP symbols_device_opening[sizeof(device_opening) / sizeof(device_opening[0])];

static int symbols_init = 0;

void init_symbols()
{
	int i = 0;
	for(; i < num_to_handle; i++)
		symbols_to_handle[i] = install(builtins_to_handle[i]);

	i = 0;
	for(; i < num_to_handle_connections; i++)
		symbols_to_handle_connections[i] = install(builtins_to_handle_connections[i]);

	i = 0;
	for(; i < num_to_handle_prints; i++)
		symbols_to_handle_prints[i] = install(builtins_to_handle_prints[i]);

	i = 0;
	for(; i < num_env_generators; i++)
		symbols_env_generators[i] = install(env_generators[i]);

	i = 0;
	for(; i < num_device_opening; i++)
		symbols_device_opening[i] = install(device_opening[i]);

	i = 0;
	for(; i < num_calls; i++)
		symbols_calls[i] = install(calls[i]);

	symbols_init = 1;
}

int is_device_opening_call(SEXP call, SEXP env)
{
	if(!RRnR_options.allow_graphs) return 0;
	if(CDR(call) == R_NilValue) return 0;

	SEXP func_symbol = CADR(call);

	int i = 0;
	for(; i < num_device_opening; i++)
		if(symbols_device_opening[i] == func_symbol)
			return 1;

	if(func_symbol == install("C_X11"))
	{
		SEXP test = PROTECT(eval(CADDR(call), env));
		int len = strlen(CHAR(asChar(test)));
		UNPROTECT(1);

		if(len) return 1;
	}

	return 0;
}

int should_handle_call(SEXP call, SEXP args, SEXP env)
{
	if(CDR(call) != R_NilValue)
	{
		/* Ignore C calls into RRnR package with package name specified. (?, (:::, RRnR, name), ...args) */
		if(TYPEOF(CADR(call)) == LANGSXP && CADR(CADR(call)) == install("RRnR"))
		{
			SEXP func = CADDR(CADR(call));

			if(func == install("C_do_lazyload_before") || func == install("C_do_lazyload_after"))
				return 0;
		}

		/* Ignore C calls into RRnR package without package name specified. */
		else if(CADR(call) == install("C_do_lazyload_before") || CADR(call) == install("C_do_lazyload_after"))
			return 0;
	}

	/* Ignore graph drawing calls. */
	if(RRnR_options.allow_graphs && CAR(args) != R_NilValue && TYPEOF(CAR(args)) == VECSXP)
	{
		SEXP native_symbol_info = CAR(args);
		SEXP dll_info = VECTOR_ELT(native_symbol_info, 2);
		SEXP package = VECTOR_ELT(dll_info, 0);
		const char * package_name = CHAR(asChar(package));

		if(strcmp(package_name, "grDevices") == 0
		|| strcmp(package_name, "graphics") == 0)
			return 0;
	}

	return 1;
}

int should_handle_builtin(SEXP call)
{
	if(!symbols_init)
		init_symbols();

	SEXP func_symbol = CAR(call);
	//const char * func_name = CHAR(PRINTNAME(func_symbol));

	int i = 0;
	for(; i < num_to_handle; i++)
		if(symbols_to_handle[i] == func_symbol)
			return 1;

	if(!RRnR_options.allow_connections)
	{
		i = 0;
		for(; i < num_to_handle_connections; i++)
			if(symbols_to_handle_connections[i] == func_symbol)
				return 1;

		if(!RRnR_options.allow_prints)
		{
			i = 0;
			for(; i < num_to_handle_prints; i++)
				if(symbols_to_handle_prints[i] == func_symbol)
					return 1;
		}
	}

	return 0;
}

int is_env_generator(SEXP call)
{
	if(!symbols_init)
		init_symbols();

	SEXP func_symbol = CAR(call);

	int i = 0;
	for(; i < num_env_generators; i++)
		if(symbols_env_generators[i] == func_symbol)
			return 1;

	return 0;
}

int is_write(SEXP call)
{
	if(!symbols_init)
		init_symbols();

	SEXP func_symbol = CAR(call);

	int i = writes_offset;
	for(; i < num_to_handle_prints; i++)
		if(symbols_to_handle_prints[i] == func_symbol)
			return 1;

	return 0;
}

int detect_call_type(SEXP call, int call_type)
{
	if(call_type == RRNR_DOTCALL)
		return RRNR_DOTCALL;

	if(!symbols_init)
		init_symbols();

	SEXP func_symbol = CAR(call);

	/* External-calling builtins should be treated as .Call. */
	int i = 0;
	for(; i < num_calls; i++)
		if(symbols_calls[i] == func_symbol)
			return RRNR_DOTCALL;

	return RRNR_BUILTIN;
}

/* Detects the type of the call and sets appropriate flags. */
int detect_flags(SEXP call, SEXP op, SEXP args, SEXP env, int call_type)
{
	int flags = 0;

	call_type = detect_call_type(call, call_type);

	if(call_type == RRNR_BUILTIN)
	{
		/* Early-out for primitives, because there are none to be recorded. */
		if(!PRIMINTERNAL(op)) return 0;

		if(!RRnR_options.allow_connections && RRnR_options.allow_prints && CAR(call) == install("sink"))
			flags = SHOULD_BE_HANDLED | IS_SINK;
		else if(!RRnR_options.allow_connections && RRnR_options.allow_prints && is_write(call))
			flags = SHOULD_BE_HANDLED | IS_WRITE;
		else if(should_handle_builtin(call))
			flags = SHOULD_BE_HANDLED;
		else if(is_env_generator(call))
			flags = IS_ENV_GENERATOR;

		flags |= IS_INTERNAL;
	}

	if(call_type == RRNR_DOTCALL)
	{
		if(is_device_opening_call(call, env))
			flags = SHOULD_BE_HANDLED | IS_DEVICE_OPENING;
		else if(should_handle_call(call, args, env))
			flags = SHOULD_BE_HANDLED | CAN_PRODUCE_CALLBACK;
	}

	return flags;
}
