print_random <- function()
{
	invisible(.Call(C_do_print_random))
}

reset_print_random <- function(seed)
{
	invisible(.Call(C_reset_print_random, seed))
}

get_print_random_counter <- function()
{
	return(.Call(C_get_print_random_counter))
}

call_runif <- function(seed)
{
	invisible(.Call(C_call_runif, seed))
}

simple_return <- function(obj)
{
	return(.Call(C_simple_return, obj))
}

test_rprintf_capture <- function()
{
	invisible(.Call(C_test_rprintf_capture))
}

throw_an_error <- function()
{
	invisible(.Call(C_throw_an_error))
}

call_throw_an_error <- function()
{
	invisible(.Call(C_call_throw_an_error))
}
