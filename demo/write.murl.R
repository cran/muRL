data(murljobs)

## Create mailmerge.txt required for LaTeX import
write.murl(murljobs)

## Specify salutation, valediction options (overwrites previous mailmerge.txt)
write.murl(murljobs, file.name = "mailmerge.txt", salutation = "Greetings", 
				sal.punct = ",", valediction = "Truly Yours,", include.opening = FALSE)

## Specify opening line also (overwrites previous mailmerge.txt)
write.murl(murljobs, file.name = "mailmerge.txt", salutation = "Greetings", 
				sal.punct = ",", valediction = "Truly Yours,", 
				opening = "I am applying for the job in", include.opening = TRUE)