write.murl <- function(object, file.name = "mailmerge.tex", salutation = "Dear", sal.punct = ":", address.string = "123 Venus Flytrap Way\\\\Cincinnati, OH 45201\\\\ \\texttt{jfever@wkrp.edu}\\\\ \\texttt{http://www.wkrp.edu/jfever}\\\\513-555-5664", letter.file = NULL, letter.text = NULL, valediction = "Sincerely,", signature = "Johnny Fever", opening = "", include.opening = FALSE, verbose = TRUE){
	
	##empty contents for .txt file
	empty <- ""
	## create the empty .txt file
	write(empty, file = file.name)
	
	write("%% Sample template for muRL by Moore and Reeves.  By pdflatex-ing this document, a pdf file will be generated, which will include letters for each address %specified in your jobs spreadsheet, and a sheet of mailing labels on the last page of the document.  See documentation for further details.

	write(address.string, file = file.name, append = T)
	write("} %end of return address", file = file.name, append = T)
	write("\\date{March 1, 1972} %alternatively, can use \\date{\\today} to specify today's date.", file = file.name, append = T)
	write(paste("\\signature{", signature, "} %your name, which will follow the valediction.", sep = ""), file = file.name, append = T)
	write("\\begin{document}	
	if(!is.null(letter.file)){
		letter.file <- path.expand(letter.file)
		write(paste("\\input{", letter.file, "}}\n\n% begin letter and label generation", sep = ""), file = file.name, append = T)	
	}
	if(!is.null(letter.text)){	
		write(paste(letter.text, "} %end of define body.\n\n%begin letter and label generation", sep=""), file = file.name, append = T)
	}
	
	if(is.null(letter.file) && is.null(letter.text)){
		write("
	}
	
	## get address columns
	add.col <- grep("address", names(object))
	
	for(i in 1:nrow(object)){
		
		##pos.str <- paste(sal.punct, "}\n ", opening, " ", sep="")

		tmp <- object[i, add.col]
		tmp <- tmp[tmp != ""]
	
		form <- paste("\\begin{letter}\n{", 
			object$title[i], " ",
			object$fname[i], " ",
			object$lname[i], "\\\\",	
			paste(tmp, "\\\\", sep="", collapse = ""),
			object$city[i], " ",
			object$state[i], " ",
			object$zip[i], 
			paste("}\n\\opening{", salutation, sep=""),
        " ",
        object$title[i], " ",
        object$lname[i],
        sal.punct, 
        "}\n ",
        if(include.opening == TRUE){
        		paste(opening, " ",
        		object$position[i],
        		" in the ",
        		object$dept[i],
        		" at ",
        		object$institution[i],
        		".", sep = "")
        	},	       		
        paste(" \\body \n\\closing{", valediction, "} \n\\end{letter}\n", sep=""),
        sep="")
        
      ## " \\body \n\\closing{\\myclosing} \n\\end{letter}\n",     
      write(form, file = file.name, append = T)
   }
   
   write("\\end{document}", file = file.name, append = T)
   
   if(verbose==TRUE){
	   	cat("Data stored as file `", file.name, "'.  \nThe current working directory is ", getwd(), "\n", sep="")
	}
}