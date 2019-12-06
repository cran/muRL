apsahtml2csv <- function(directory, file.name, file.ext = ".htm", verbose = TRUE){
	
	jobs <- dir(directory, full.names = TRUE)[grep(file.ext, dir(directory))]
	
	for(i in 1:length(jobs)){
		
	  if(verbose == TRUE){
	    cat("Starting ", jobs[i], "\n")
	  }
		write("\n", jobs[i], append=T)
		
		data <- readLines(jobs[i])
		
		data <- data[grep("Job Detail</legend>", data):grep("</div></div>", data)]
		
		assign("listingid", getfield(data, "Job ID", 3, ".*>(.*)<.*", "\\1"))
		assign("startdate", getfield(data, "Start Date", 3, ".*>(.*)<.*",  "\\1"))
		if(length(startdate) > 1){
		  startdate <- startdate[1]
		}
		
		assign("deadline", getfield(data, "Application Deadline", 3, ".*>(.*)<.*", "\\1"))
		if(length(deadline) > 1){
			deadline <- deadline[1]
		}
		
		assign("position", getfield(data, "Title:", 4, ".*>(.*)<.*", "\\1"))
		position <- tolower(position)
		
		assign("subfield", getfield(data, "Subfield:", 4, ".*>(.*?)<.*>(.*?)<.*>(.*?)<.*>", "\\1"))
		if(length(subfield) > 1){
			subfield <- subfield[1]
		}
		
		assign("dept", getfield(data, "Department:", 4, ".*>(.*)<.*", "\\1"))
		assign("inst", getfield(data, "Company:", 4, ".*>(.*)<.*", "\\1"))
		
		assign("region", getfield(data, "Region:", 4, ".*>(.*)<.*", "\\1"))
		region <- str_trim(region)
		
		assign("salary", getfield(data, "Salary Range:", 5, ".*>(.*)<.*", "\\1" ))
		
		assign("desc", getdesc(data))
		assign("contact", getfield(data, "Contact:", 4, ".*>(.*)<.*", "\\1"))
		if(length(contact) == 0){
			contact <- ""
		}
		assign("phone", getfield(data, "Phone:", 4, ".*>(.*)<.*", "\\1"))
		if(length(phone) == 0){
			phone <- ""
		}
		
		subfield <- tolower(subfield)
		subfield <- gsub("american", "American", subfield)
		
		## 2015: some employers omit address:
		if(length(grep("Address:", data) > 0)){
			address <- data[grep("Address:", data):length(data)]
			## Get address line:
			address <- address[grep("<span id=", address)]
			
			## Remove HTML:
			begin.address <- stringr::str_locate(address, c(">", "</span"))[1, "end"] + 1
			end.address <- stringr::str_locate(address, c(">", "</span"))[2, "end"] - 6
			address <- substring(address, begin.address, end.address)
			address <- gsub("#", "\\\\#", address)
			
			## Split address into rows of address:
			address.split <- strsplit(address, "<br/>")[[1]]
			
			## Store ZIP code:
			zip.chars <- gregexpr("[0-9|-]", address.split[length(address.split)])[[1]]
			zip <- substring(address.split[length(address.split)], zip.chars[1], zip.chars[length(zip.chars)])
			zip <- gsub(" ", "", zip)
			
			## Get only letters:
			citystate.chars <- gregexpr("[a-z|A-Z]", address.split[length(address.split)])[[1]]
			citystate <-substring(address.split[length(address.split)], citystate.chars[1], citystate.chars[length(citystate.chars)])
			state <- substr(citystate, nchar(citystate)-1, nchar(citystate))
			
			if(state == ", "){state<-""}
			
			city <- substr(citystate,1, nchar(citystate)-3)
		}else{ ## if "Address:" is missing:
			city <- state <- zip <- ""
		}
		
		## Clean up description:
		desc <- gsub("pre style='font-family: Trebuchet MS,Arial; white-space: pre-wrap; width:100%;'>", "", desc)
		desc <- gsub("span style='font-family: Trebuchet MS,Arial; white-space: pre-wrap; width:100%;'>", "", desc)
		desc <- gsub("dnn_ctr4356_ViewJobBank_ViewJob_lb_JobText", "", desc)
		desc <- gsub("<span id=", "", desc)
		desc <- gsub("<br />", " ", desc)
		desc <- gsub("><", "", desc)
		desc <- str_trim(desc)
		
		## Create final data frame for output
		df <- data.frame(listingid = listingid, subfield = subfield, startdate = startdate, deadline = deadline, position = position, dept = dept,
		                 institution = inst, region = region, salary = salary, contact = contact, phone = phone, city = city, state = state, 
		                 zip = zip, desc = desc)
		
		if(length(grep("Address:", data) > 0)){
			## Make Address
			add.vec <- paste("address", 1:(length(address.split) - 1), sep="")
			
			for(j in 1:length(add.vec)){
				df <- cbind(df, address.split[j])
			}
			
			names(df)[(ncol(df) - (length(add.vec) - 1)):ncol(df)] <- add.vec
		}
		
		if(i == 1){
			myjobs <- df
		}else{
			myjobs <- merge(myjobs, df, all = TRUE)
		}
	}
		
	write.csv(myjobs, file = file.name, row.names = FALSE, na = "")

	if(verbose == TRUE){
	  cat("Data stored as file `", file.name, "'.  \nThe current working directory is ", getwd(), "\n", sep="")
	 }
}
