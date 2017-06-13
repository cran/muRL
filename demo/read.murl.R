## Specify path to .csv database of sample addresses
fpath <- system.file("extdata", "murljobs.csv", package = "muRL")

murljobs <- read.murl(fpath)
