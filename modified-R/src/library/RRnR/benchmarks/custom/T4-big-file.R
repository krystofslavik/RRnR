run <- function()
{
	sum <- 0
	con <- file(paste(sep="/", dir, "custom/T4-big-file.txt"), "r")
	while(TRUE) {
		line <- readLines(con, n = 1)
		if(length(line) == 0) break()
		sum <- sum + nchar(line)
	}
	
	close(con)
	print(sum)
}
