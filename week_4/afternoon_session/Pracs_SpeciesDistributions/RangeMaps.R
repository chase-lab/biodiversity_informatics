#######################################
## this is for playing with range maps:
# download range maps from some database; read range maps; map range maps; rasterize range maps etc.
# and download GBIF occurrences and constructure range maps;
# this r-script was modified from the Course <Macroecology and global change> lead by Damaris Zurell. Thanks!

rm(list = ls())

# set your working directory
setwd("C:/Dropbox/iDiv/Courses/BScBioinformatics_2022/Pracs_SpeciesDistributions")

#install and load packages
packages <- c("raster","maps","BIEN","sp", "rgeos","letsR", "rgbif", "maptools", "alphahull")
package.check <- lapply(packages, FUN=function(x)
  {
  if(!require(x, character.only=TRUE)){
    install.packages(x, dependencies=TRUE)
    library(x, character.only=TRUE)
    }
  }
)


####################
## 1 Obtaining range maps
# 1.1 IUCN range maps
# you can download expert range maps of mammals, birds, amphibians, reptiles, and freshwater fishes and some plants and marine species
# from the IUCN website (https://www.iucnredlist.org/resources/spatial-data-download)
# they are fully open accessible, only require registration

# Load the shapefile of one species using the raster package
shrew <- shapefile('data/IUCN_Sorex_alpinus.shp')
shrew

# Plot the Central Europe using the maps package
map('world', xlim=c(5,30), ylim=c(40,55))

# Overlay the range of the Alpine Shrew
plot(shrew, col='red', add=T)


# Read shapefile for multiple mammal species (a subset of MAMMALS download from IUCH)
mammals <- shapefile('data/MAMMALS_subset.shp')

# this is a SpatialPolygonsDataFrame with the range polygons of many species. The attribute table contains information for each polygon
mammals
head(mammals@data)
unique(mammals@data$binomial) # names of species


## We can search for specific species or species groups in the attribute table
# Show all entries for the species 'Lynx lynx'
subset(mammals@data, binomial=='Lynx lynx')

# Range map of the Eurasian lynx
lynx_lynx <- mammals[mammals@data$binomial=='Lynx lynx',]

# Map the range
map('world')
plot(lynx_lynx, col='red', add=T)


## 1.2 BIEN range maps
# The BIEN database (Botanical Information and Ecology Network) contains many range maps on plants in America
# Load the range map for the monkey puzzle
monkey_puzzle <- BIEN_ranges_load_species('Araucaria_araucana')
monkey_puzzle

# Map
map('world', xlim = c(-180,-20), ylim = c(-70,80))
plot(monkey_puzzle, col='red', add=T)



####################
## 2 Working with range maps
## 2.1 Range size and range centre

# we can calculate area of polygon (range) using the function area from raster package
# Range area of alpine shrew in square meters (if the CRS is in long/lat format)
area(shrew)

# Range area of alpine shrew in square kilometers:
area(shrew)/1000000

# Range area of monkey puzzle n square kilometers:
area(monkey_puzzle)/1000000

# calculate the centre of gravity or range centroid from the spatial polygons.
gCentroid(shrew)

# Map the species range and add the centroid to the map
map('world',xlim=c(5,30), ylim=c(40,55))
plot(shrew, col='red', add=T)
# the centroid is not in the range as it has several patches
plot(gCentroid(shrew), col='blue',add=T,lwd=3)


# Let's crop the range polygons to South America (specific areas you interested in)
monkey_puzzle_SAm <- gIntersection(monkey_puzzle, as(extent(-85, -30, -55, 5), "SpatialPolygons"))

# Map the range and range centroid
map('world', xlim = c(-180,-20), ylim = c(-60,60))
plot(monkey_puzzle_SAm,col='red',add=T)
plot(gCentroid(monkey_puzzle_SAm), col='blue', add=T, lwd=3)


## 2.2 Rasterizing range maps
# For many applications in macroecology, we need raster of polygons in specific resolutions.

## 2.2.1 Rasterizing range maps with raster
# By default, raster() will create a 1 degree resolution map in the *WGS 84* coordinate system (lon/lat).
r_1deg <- raster()
r_1deg

# transfer the polgyon data to the raster cells
shrew_1deg <- rasterize(shrew, r_1deg)
shrew_1deg

map('world',xlim=c(5,30), ylim=c(40,55))
plot(shrew, col='red', add=T)
plot(shrew_1deg, add=T, alpha=0.8, legend=F)
# note: some areas of polygon are not transfer to raster cells as they don't covers the center of a raster cell


# Define an empty raster of the world at 2 degree spatial resolution
r_2deg <- raster(res=2)

# Rasterize the eurasian lynx data
lynx_lynx_2deg <- rasterize(lynx_lynx, r_2deg, fun = "last")

# Map the occupied grid cells
plot(lynx_lynx_2deg)
# note: raster contains values > 1 as lynx_lynx SpatialPolygonsDataFrame contain multiple multiple polygons and
# the raster package will assign the index of the attribute table row as the cell value

# transfere all values into 1
values(lynx_lynx_2deg)[!is.na(values(lynx_lynx_2deg))] <- 1

# Map the occupied cells
map('world')
plot(lynx_lynx_2deg, add=T, legend=F)


# 2.2.2 Rasterising range maps with package letsR

# The lets.presab() function expects specific column names in the Polygons data frame
colnames(monkey_puzzle@data) <- "binomial"

# The lets.presab() function require a SpatialPolygonsDataFrame and a specific column (binomial) in the Polygons data frame
monkey_puzzle_SAm <- SpatialPolygonsDataFrame(monkey_puzzle_SAm, data.frame("binomial" = "Araucaria_araucana"))

# We set the resolution to 1 degree (the default) and restrict the spatial extent to South America
r_monkey_puzzle <- lets.presab(monkey_puzzle_SAm, resol=1, xmn = -100, xmx = -20, ymn = -57, ymx = 15)

# Map the range and range centroid
map('world', xlim = c(-100, -20), ylim = c(-60, 15))
plot(monkey_puzzle_SAm, col='red', add=T)
plot(r_monkey_puzzle, add=T, alpha=1, legend=F)


## 2.2.3 Bulk-rasterising multiple species range maps with letsR
# Extract the available Pinus species names
pinus_names <- BIEN_ranges_genus("Pinus", match_names_only = T)[,1]
pinus_names

# Download the range maps for all Pinus species
pinus <- BIEN_ranges_load_species(pinus_names)

# Format the column names and rasterise
colnames(pinus@data) <- "binomial"
r_pinus <- lets.presab(pinus, resol=1, xmn = -170, xmx = -10, ymn = -57, ymx = 60)

# Plot species richness
plot(r_pinus)


# Map distribution of individual species
par(mfrow=c(1, 2)) # 6 8
plot(r_pinus, name = "Pinus_albicaulis")
plot(r_pinus, name = "Pinus_caribaea")
par(mfrow=c(1, 1))

# Look at structure of the object and at the presence-absence matrix
str(r_pinus, 1)

head(r_pinus$Presence_and_Absence_Matrix)



####################
## 3 Use occurrences from GBIF

## 3.1 get GBIF occurrences and map them 
# We will find the occurrences of the species lynx lynx
# Use name_suggest to find the species ID in GBIF. Just make sure the name can be identified by GBIF
# You can also use the species names directly
lynx_gbif_name <- name_suggest(q='lynx lynx', rank='species')$data
lynx_gbif_name
lynx_gbif_key <- lynx_gbif_name$key[1]

# Search GBIF occurrences. There are many parameters to filter data
# Here we only restrict occurrences with coordinates and distributed in Erope.
lynx_gbif_occ <- occ_search(taxonKey=lynx_gbif_key, hasCoordinate=TRUE, continent = 'Europe', limit=1000)$data
lynx_gbif_occ

# look some interested columns
lynx_gbif_occ[, c("scientificName", "decimalLatitude", "decimalLongitude", "basisOfRecord", "year", "country" )]
lynx_gbif_occ2 <- lynx_gbif_occ[, c("scientificName", "decimalLatitude", "decimalLongitude", "basisOfRecord", "year", "country" )]

# transfer the table into SpatialPointsDataFrame
coordinates(lynx_gbif_occ2) <- ~decimalLongitude + decimalLatitude
projection(lynx_gbif_occ2) <- "+proj=longlat +datum=WGS84 +no_defs "
lynx_gbif_occ2

# Map distribution occurrences
map('world', xlim=c(-15, 60), ylim=c(30, 75), fill = TRUE, col = "gray")
plot(lynx_gbif_occ2, col = "red", add =TRUE)


## transfer occurrences into raster cells
# create a 2 degree resolution raster within the extent of Europe 
r_2deg_eu <- raster(xmn =-15, xmx =60, ymn =30, ymx =75, res = 2)

# transfer the points data to the raster cells: calculate the number of occurrences in each cell
lynx_gbif_2deg <- rasterize(x = lynx_gbif_occ2, y = r_2deg_eu, field = "year", fun='count')
lynx_gbif_2deg

# map the raster of number of occurrences
map('world', xlim=c(-15, 60), ylim=c(30, 75), fill = TRUE, col = "gray")
plot(lynx_gbif_2deg, add =TRUE, col = topo.colors(n = 5, rev =TRUE))



## 3.1  generate range map based on occurrence points

# input my defined functions, which will be used to construct alpha hulls
source("self_defined_functions/99_ah2sp.R")
source("self_defined_functions/99_ah_range.R")

# the function require the x (longitude) and y (latitude)
lynx_gbif_xy <- unique(lynx_gbif_occ[, c("decimalLongitude", "decimalLatitude")])
colnames(lynx_gbif_xy) <- c("x", "y")

# generate alpha hulls.The parameter alpha will affect the constructed alpha hulls: a higher value of alpha, a larger hull
data(wrld_simpl)
lynx_gbif_ahull <- ah_range(xy=lynx_gbif_xy, alpha=10, buff=20, exclude_map = wrld_simpl, is.write=FALSE) # the warnings doesn't matter
lynx_gbif_ahull

# maps the constructed range map
map('world', xlim=c(-15, 60), ylim=c(30, 75), fill = TRUE, col = "gray")
plot(lynx_gbif_ahull[[1]], add =TRUE, col = "blue")
# add occurrence points
plot(lynx_gbif_occ2, col = "red", add =TRUE)

