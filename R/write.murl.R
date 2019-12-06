write.murl <- function(object, file.name = "mailmerge.tex", salutation = "Dear", 
                       sal.punct = ":", 
                       address.string = "123 Venus Flytrap Way\\\\Cincinnati, 
OH 45201\\\\ \\texttt{jfever@wkrp.edu}\\\\ 
\\texttt{http://www.wkrp.edu/jfever}\\\\513-555-5664",
                       pad_if_zip4 = TRUE,
                       date = "\\today", letter.file = NULL, letter.text = NULL, 
                       valediction = "Sincerely,", signature = "Johnny Fever", 
                       opening = "", include.opening = FALSE,
                       closing = "", include.closing = FALSE,
                       contact_me = TRUE,
                       margin_geometry = NULL,
                       verbose = TRUE){
	
	##empty contents for .tex file
	empty <- ""
	## create the empty .tex file
	write(empty, file = file.name)
	
	write("%% Sample template for muRL by Moore and Reeves.  
	%% By pdflatex-ing this document, a pdf file will be generated, which will include letters for each address %specified in your jobs spreadsheet, and a sheet of mailing labels on the last page of the document.  
	%% See documentation for further details.

%% User must define body, return address, and specify any formatting preferences 
%% in the preamble of this document.  

\\documentclass[11pt]{letter} 
\\usepackage{graphicx} % required for envab package.
\\usepackage[avery5164label]{envlab} % generates labels appropriate for avery brand, number 5164.  More label formats are available.  Generate barcodes for ZIP codes ensuring faster delivery.  See http://ctan.tug.org/get/macros/latex/contrib/envlab/envlab.pdf for more information.
\\makelabels % generates mailing labels and includes them after letters.  Comment this out if you do not wish to generate labels.

%%margin options", file = file.name, append = TRUE)
	
	if(!is.null(margin_geometry)){
	  write(paste0("\\usepackage[top=", margin_geometry[1], "in,", "bottom=", margin_geometry[2], "in,",
	               "left=", margin_geometry[3], "in,", "right=", margin_geometry[4], "in]{geometry}"),
	        file = file.name, append = TRUE)
	}
	
	write("%%\\usepackage{fullpage}
%%font options
%\\usepackage{times}

\\address{%your return address.  If you are using letterhead, you should adjust the top margin and you should omit the street address here.
%\\vspace{2in}\\ %add white space here if using letterhead.", file = file.name, append = T)

	write(address.string, file = file.name, append = T)
	write("} %end of return address", file = file.name, append = T)
	write(paste("\\date{", date, "}", sep = ""), file = file.name, append = T)
	write(paste("\\signature{", signature, "} %your name, which will follow the valediction.", sep = ""), file = file.name, append = T)
	write("\\begin{document}	
\\def
\\body{%this defines the text of the body of your cover letter.  If specified by the user, the opening line is generated in R using muRL and is not show in the tex below.", file = file.name, append = T)
	if(!is.null(letter.file)){
		letter.file <- path.expand(letter.file)
		write(paste("\\input{", letter.file, "}}\n\n% begin letter and label generation", sep = ""), file = file.name, append = T)	
	}
	if(!is.null(letter.text)){	
		write(paste(letter.text, "} %end of define body.\n\n%begin letter and label generation", sep=""), file = file.name, append = T)
	}
	
	if(is.null(letter.file) && is.null(letter.text)){
		write("
I will complete my Ph.D. at WKRP University in the Department of Musicology and will defend my dissertation in June 1972.

My dissertation examines \\ldots

In addition to my dissertation, I have also worked on \\ldots 

In addition to my research, I believe I could be an an asset to your department through the courses I could offer \\ldots

I have included with this letter, two chapters of my dissertation, my \\emph{curriculum vitae}, an outline of my dissertation, and a summary of my teaching experiences.  You will also be receiving letters of recommendation from Professors Less Nessman, Jennifer Marlowe, and Herb Tarlek, which will arrive under separate cover.   Thank you for your consideration of my application.  I look forward to hearing from you.
} %end of define body.\n\n%begin letter and label generation", file = file.name, append = T)
	}
	
	# get address columns
	add.col <- grep("address", names(object))
	
	for(i in 1:nrow(object)){

		tmp <- object[i, add.col]
		tmp <- tmp[tmp != ""]
		
		# Pad short ZIP codes with a 0:
		if(pad_if_zip4 == TRUE){
		  if(nchar(object$zip[i]) %in% c(4, 9)){
		    object$zip[i] <- paste0("0", object$zip[i])
		    warning(paste0("In row ", i, ", ZIP code '", object$zip[i], 
		    "' was created by adding a leading 0 (zero)."))
		  }
		}
		
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
        		      " in ", object$subfield[i],
        		      " in the ", object$dept[i],
        		      " at ", object$institution[i],
        		      ".", sep = "")
        	},	       		
      
			  paste(" \\body \n"),
			
			  if(include.closing == TRUE){
			    
			    if(contact_me == TRUE){
			      contact_phrase <- "  Please don't hesitate to contact me if more information would be helpful."
			    }else{contact_phrase <- ""}
			    
			    paste(closing, " ",
			        object$position[i],
			        " in ",
			        object$subfield[i],
			        " at ",
			        object$instShort[i],
			        ".", contact_phrase, sep = "")
			  },	       		
        paste("\\closing{", valediction, "} \n\\end{letter}\n", sep=""),
        
			sep="")
        
      ## " \\body \n\\closing{\\myclosing} \n\\end{letter}\n",     
      write(form, file = file.name, append = T)
	}
	
	write("\\end{document}", file = file.name, append = T)
	
	if(verbose == TRUE){
	   	cat("Data stored as file `", file.name, "'.  \nThe current working directory is ", getwd(), "\n", sep="")
	}
}