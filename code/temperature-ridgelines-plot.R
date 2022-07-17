library(tidyverse)

url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
download.file(url, destfile = "gistemp250_GHCNv4.nc.gz")