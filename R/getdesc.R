getdesc <- function(data){
	tmp <- data[(grep("Job Description:", data) + 5) : (grep("</span></span>", data))]
	tmp <- paste(tmp, collapse = " ")
	return(tmp)
}
