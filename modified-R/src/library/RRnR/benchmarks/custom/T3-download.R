run <- function()
{
	download.file("https://cran.r-project.org/", "tmp.file", quiet = TRUE)
	file.remove("tmp.file")
}
