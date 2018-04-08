run <- function()
{
	# sleep for 1 second
	switch(Sys.info()[['sysname']],
		Windows= {system("ping 127.0.0.1 -n 2 > nul", intern = TRUE)},
		Linux  = {system("sleep 1", intern = TRUE)},
		Darwin = {system("sleep 1", intern = TRUE)})
}
