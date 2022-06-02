#-----------------------------------------------------------------------------#
# setup routine
#-----------------------------------------------------------------------------#

setwd('')

# access required R packgaes
require(CoordinateCleaner)
require(countrycode)
require(data.table)

ifile = '' # input file with original samples
ofile = '' # output file with filtered samples

#-----------------------------------------------------------------------------#
# load input samples
#-----------------------------------------------------------------------------#

ids = data.table::fread(ifile, sep='\t', colClasses='character')

#-----------------------------------------------------------------------------#
# estimate coordinate precision (only if no precision is provided)
#-----------------------------------------------------------------------------#

# find samples that don't report precision
ind = which(ids$coordinateUncertaintyInMeters == '')

# estimate precision of longitudes
x = sapply(ids$decimalLongitude[ind], function(i) {
  tmp = strsplit(i, '[.]')[[1]]
  if (length(tmp) == 1) {return(0)} else {
    return(length(unique(strsplit(tmp[2], '')[[1]])))
  }
})

# estimate precision of latitudes
y = sapply(ids$decimalLatitude[ind], function(i) {
  tmp = strsplit(i, '[.]')[[1]]
  if (length(tmp) == 1) return(0) else {
    return(length(unique(strsplit(tmp[2], '')[[1]])))
  }
})

# find worse precision for each lat/lon pairs
udf = apply(data.frame(x=x, y=y), 1, max)

# 0 decimal places (precision of 100 km)
pos = which(udf == 0)
ids$coordinateUncertaintyInMeters[ind[pos]] = 100000

# 1 decimal places (precision of 10 km)
pos = which(udf == 1)
ids$coordinateUncertaintyInMeters[ind[pos]] = 10000

# 2 decimal places (precision of 1 km)
pos = which(udf == 2)
ids$coordinateUncertaintyInMeters[ind[pos]] = 1000

# 3 decimal places (precision of 100 m)
pos = which(udf == 3)
ids$coordinateUncertaintyInMeters[ind[pos]] = 100

# 4 or more decimal places (precision of 10 m)
pos = which(udf >= 4)
ids$coordinateUncertaintyInMeters[ind[pos]] = 10

#-----------------------------------------------------------------------------#
# test GBIF observations with coordinateClearner
#-----------------------------------------------------------------------------#

# convert coordinates to numeric
ids$decimalLongitude = as.numeric(ids$decimalLongitude)
ids$decimalLatitude = as.numeric(ids$decimalLatitude)

# convert country code from ISO2 to ISO3 (needed for coordinateClearner)
ids$countryCode = countrycode(ids$countryCode, 'iso2c', 'iso3c')

# flag dubious observations based on their location
flags <- clean_coordinates(x = ids, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal", 
                                     "gbif", "institutions", "zeros", 
                                     "countries"))

#-----------------------------------------------------------------------------#
# save output
#-----------------------------------------------------------------------------#

ods = ids[flags$.summary,]
write.csv(ods, ofile, row.names=F)
