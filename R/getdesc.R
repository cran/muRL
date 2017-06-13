getdesc <- function(data){
	
	#tmp <- data[(grep("Job Description:", data) + 4) : (grep(">Contact:<", data) - 9)]	
  
  tmp <- data[(grep("Job Description:", data) + 4) : (grep("</span>", data))]	
  
	tmp <- paste(tmp, collapse = " ")
	return(tmp)
}