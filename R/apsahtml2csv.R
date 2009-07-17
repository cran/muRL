apsahtml2csv <- function(directory, file.name, file.ext = ".htm"){

	jobs <- dir(directory, full.names = TRUE)[grep(file.ext, dir(directory))]
	
	for(i in 1:length(jobs)){
		
		data<-readLines(jobs[i])
		
		data<-data[grep("General Job Listing Information", data):grep("<!-- End main page content -->", data)]

		assign("listingid", getfield(data, "Listing ID", 1, ".*>(.*)<.*", "\\1"))
		assign("dateposted", getfield(data, "Date Posted", 2, "\t", ""))
		assign("typeofinst", getfield(data, "Type of Insitution", 1, ".*>(.*)<.*", "\\1" ))
		assign("position", getfield(data, "Title of Position", 1, ".*>(.*)<.*", "\\1"))
		assign("startdate", getfield(data, "Starting Date", 1, ".*>(.*)<.*",  "\\1"))
		assign("salary", getfield(data, "Salary:", 1,".*>(.*)</td.*", "\\1" ))
		assign("region", getfield(data, "Geographic Region", 3, "\t\t", ""))
		assign("desc", getfield(data, "Complete Position Description", 8, "\t", ""))
		assign("email", getfield(data, "E-Mail:", 1, ".*mailto:(.*)\".*", "\\1"))
		assign("phone", getfield(data, "Phone:", 1, ".*>(.*)<br>.*", "\\1"))
		assign("contact", getfield(data, "Name of Representative", 1, ".*>(.*)<br>.*", "\\1"))
		assign("web", getfield(data, "Departmental Web Address:", 2, ".*href=\\\"(.*)\\\" tar.*", "\\1"))
		assign("inst", getfield(data, "Institution Name", 1, ".*>(.*)<br>.*", "\\1"))
		assign("dept", getfield(data, "Name of Department", 1, ".*>(.*)<br>.*", "\\1"))

		address <- data[grep("Mailing Address:", data):length(data)]
		address <- gsub("<br>", "", address)
		address <- address[-grep("\t", address)]
		address <- address[-grep("<", address)]
		address <- address[!address==""]
		address <- gsub("#", "\\\\#", address)

		zip <- address[length(address)]
		citystate <- address[length(address)-1]
		state <- substr(citystate,nchar(citystate)-1, nchar(citystate))
		
		if(state == ", "){state<-""}
		
		city <- substr(citystate,1, nchar(citystate)-4)
		
		df <- data.frame(listingid = listingid, dateposted = dateposted, typeofinst = typeofinst, position = position, startdate = startdate, salary = salary, region = region, institution = inst, dept = dept, contact = contact, city = city, state=state, zip = zip, phone = phone, www = web, desc = desc)

		#Make Address
		add.vec <- paste("address", 1:(length(address)-2), sep="")
		
		for(j in 1:length(add.vec)){
			df <- cbind(df, address[j])
		}

		names(df)[(ncol(df) - (length(add.vec) - 1)):ncol(df)] <- add.vec

		if(i==1){myjobs<-df}else{myjobs<-merge(myjobs,df, all = TRUE)}
	}

	write.csv(myjobs, file = file.name, row.names = FALSE, na = "")
}