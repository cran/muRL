## Call murl object of sample addresses
data(murljobs)
zip.plot(murljobs)

## Read .csv to murl object
murljobs <- read.murl(system.file("extdata", "murljobs.csv", package = "muRL"))
## Specify US state to map
zip.plot(murljobs, map.type = "state", region = "maryland")

rm(stateMapEnv)