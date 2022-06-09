#-----------------------------------------------------------------------------#
# Morning exercise
#-----------------------------------------------------------------------------#

## Intro

# In this first exercise we will look at the forest and cropland changes in Germany during the period 1992-2020 by using the ESA-CCI time-series 
# (see https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=form). 
# This is a global product at 300 m spatial resolution that includes also other land-use and land-cover classes as: shrubland, wetland, water body, urban and built-up area, permanent snow and ice, 
# grassland and bare land. You can find all the classes in Appendix A of this documentation: 
# https://datastore.copernicus-climate.eu/documents/satellite-land-cover/D5.3.1_PUGS_ICDR_LC_v2.1.x_PRODUCTS_v1.1.pdf
                                                                                                                                            
# Specifically we will do the following:
                                                                                                                                              
# - A) Visual changes: Look if changes can be visually identified
# - B) Numerical changes: Look at the changes numerically
# - C) Spatial visual changes: Look visually at the forest and cropland changes per state in Germany
# - D) Spatial numerical changes: Look at the spatial changes numerically
# - E) Distribution ground-truth samples: Select and plot a subset of the available samples to quantify accuracy in 2020
# - F) F1: measure F1 score for forest and cropland, and plot the locations where correct and wrong classes were observed in the map
# - G) Answer questions: .Rmd file

#-----------------------------------------------------------------------------#
# Load libraries and path
#-----------------------------------------------------------------------------#

library(raster)
library(sf)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)
library(stringr)
library(data.table)
library(reshape2)
library(tidyr)
library(ggpubr)
library(tidyverse)

#-----------------------------------------------------------------------------#
# A) Visual changes
#-----------------------------------------------------------------------------#

# load raster files
setwd('C:/Users/cb58hypi/Desktop/BiodiversityInformatics/data/maps/ESA_CCI/') # change to your path
temp = list.files(pattern="*.tif") # list all .tif rasters in the folder
allrasters =  stack(temp) # stack all the rasters

# load Germany country borders
borders = read_sf('C:/Users/cb58hypi/Desktop/BiodiversityInformatics/data/borders/gadm36_1.shp') # change to your path
Germany = borders[borders$NAME_0=='Germany',]

# prepare the classes of the rasters and select some of the years
mask = mask(allrasters, Germany) # select only the cells within the country borders of Germany. It will take some time: e.g. around 3 minutes
legend = read.csv('C:/Users/cb58hypi/Desktop/BiodiversityInformatics/data/class_translation.csv') # change to your path
legend_ESA = as.matrix(legend[legend$map=='ESA_CCI',c(2,4)]) # notice that we will use 1 for cropland, 10 for forest and 4 for other class

ESA_rec = reclassify(mask,legend_ESA) # re-classify the classes of the stack based on the new codes. This line will also take some time but less than 3 minutes 
subset = subset(ESA_rec, c(4,9,14,19,24,29)) # subset only years 1995,2000,2005,2010, 2015, 2020

# make plot for visual checks
levelplot(subset,col.regions=colorRampPalette(brewer.pal(9, "BrBG")))

rm(list=ls()[! ls() %in% c("Germany","ESA_rec")])

#-----------------------------------------------------------------------------#
# B) Numerical changes
#-----------------------------------------------------------------------------#

# find the number of pixels in Germany per land-use/land-cover class and year
pixel_n<-list()

for (i in 1:nlayers(ESA_rec)){
  
  name = names(ESA_rec[[i]])
  cropland =  cellStats(ESA_rec[[i]] == 1, 'sum') # find pixel number with cropland
  forest =  cellStats(ESA_rec[[i]] == 10, 'sum') # find pixel number with forest
  other =  cellStats(ESA_rec[[i]] == 4, 'sum') # find pixel number with other class
  
  df = as.data.frame(cbind(cropland, forest, other))
  df$year = as.numeric(str_remove(name, 'ESA_'))
  pixel_n [[i]] <- df
  
}

df = rbindlist(pixel_n) 

# find the changes and re-shape the table
df$cropland_change = df$cropland - df$cropland[df$year==1992]
df$forest_change = df$forest - df$forest[df$year==1992]
df$other_change = df$other - df$other[df$year==1992]

df = df[,c(4:7)]
df = df %>% pivot_longer(cols=c('cropland_change', 'forest_change','other_change'),
                         names_to='class',values_to='delta pixel number')

# make plot of the numerical changes (number of pixel changed per class during 1992-2020)
df$year<-as.character(df$year)

ggdotchart(df, 
           x = "year", 
           y = "delta pixel number", 
           color = "class", 
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
           sorting = "none", 
           add = "segments", 
           add.params = list(color = "lightgray", size = 0.1), 
           group = "class", 
           dot.size = 2 ) + geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

rm(list=ls()[! ls() %in% c("Germany","ESA_rec")])

#-----------------------------------------------------------------------------#
# C) Spatial visual changes
#-----------------------------------------------------------------------------#

# select only the two layers needed to find the change
layer_1992 =  subset(ESA_rec, c(1)) # 1992 layer
layer_2020 = subset(ESA_rec, c(29)) # 2020 layer

# create matrix for forest
matrix_forest<-c(NA,1,4,1,10,5) # give now value 5 to forest to find out if there is gain (will thus show up as -4) or loss (will thus show up as 4)
matrix_forest<-matrix(matrix_forest,nrow=3, ncol=2,byrow=TRUE)

# create matrix for cropland
matrix_cropland<-c(NA,1,4,1,1,5,10,1) # give now value 5 to cropland to find out if there was gain (will thus show up as -4) or loss (will thus show up as 4)
matrix_cropland<-matrix(matrix_cropland,nrow=4, ncol=2,byrow=TRUE)

# develop forest change raster
forest_1992 = reclassify(layer_1992,matrix_forest) # re-classify based on new matrix for change
forest_2020 = reclassify(layer_2020,matrix_forest) # re-classify based on new matrix for change
forest_change = forest_2020 - forest_1992 # find change
forest_change = mask(forest_change, Germany) # find change

# plot changes in forest
col_manual<- colorRampPalette(c('blue', 'gray96', 'red')) # red is loss, blue is gain
color_levels=3 
plot(forest_change,col=col_manual(n=color_levels))
plot(st_geometry(Germany), add = TRUE)
title(main = "Forest change 1992-2020")

# develop cropland change raster
cropland_1992 = reclassify(layer_1992,matrix_cropland)
cropland_2020 = reclassify(layer_2020,matrix_cropland)
cropland_change = cropland_2020 - cropland_1992 
cropland_change = mask(cropland_change, Germany)

# plot the changes in cropland  
col_manual<- colorRampPalette(c('blue', 'gray96', 'red')) # red is loss, blue is gain
color_levels=3 
plot(cropland_change,col=col_manual(n=color_levels))
plot(st_geometry(Germany), add = TRUE)
title(main = "Cropland change 1992-2020")

rm(list=ls()[! ls() %in% c("Germany","ESA_rec",'forest_change','cropland_change','layer_2020')])

#-----------------------------------------------------------------------------#
# D) Spatial numerical changes
#-----------------------------------------------------------------------------#

# forest changes per state
pixel_n<-list()

for (i in 1:nrow(Germany)){
  
  state = Germany[i,]
  name = as.character(state[,4])
  name = name[1]
  subset =  mask(forest_change,state)
  gain =   cellStats(subset == -4, 'sum')
  loss =   cellStats(subset == 4, 'sum')
  pixel = ncell(subset)
  gain = gain/pixel*100
  loss = loss/pixel*100
  df = as.data.frame(cbind(gain, loss, name))
  pixel_n [[i]] <- df
  
}

df = rbindlist(pixel_n) 

# re-shape table
df = gather(df, change, percentage, gain:loss, factor_key=TRUE)
df$percentage = as.numeric(df$percentage)
df$percentage = round(df$percentage, digits = 2)

# make the plot
ggplot(df, aes(x=name, y=percentage)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) + 
  facet_grid(. ~ change)+ theme(
    axis.text.x = element_text(
      angle = 45,vjust = 0.5),axis.title.x=element_blank()) + 
  geom_hline(yintercept= 0.2, colour = "red")+ geom_hline(yintercept= 0.1, colour = "red")+ylab("change percentage")+   ggtitle ('forest')

# cropland changes per state
pixel_n<-list()

for (i in 1:nrow(Germany)){
  
  state = Germany[i,]
  name = as.character(state[,4])
  name = name[1]
  subset =  mask(cropland_change,state)
  gain =   cellStats(subset == -4, 'sum')
  loss =   cellStats(subset == 4, 'sum')
  pixel = ncell(subset)
  gain = gain/pixel*100
  loss = loss/pixel*100
  df = as.data.frame(cbind(gain, loss, name))
  pixel_n [[i]] <- df
  
}

df = rbindlist(pixel_n) 

# re-shape table
df = gather(df, change, percentage, gain:loss, factor_key=TRUE)
df$percentage = as.numeric(df$percentage)
df$percentage = round(df$percentage, digits = 2)

# make plot
ggplot(df, aes(x=name, y=percentage)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) + 
  facet_grid(. ~ change)+ theme(
    axis.text.x = element_text(
      angle = 45,vjust = 0.5),axis.title.x=element_blank()) + 
  geom_hline(yintercept= 0.2, colour = "red")+ geom_hline(yintercept= 0.1, colour = "red")+ylab("change percentage") +   ggtitle ('cropland')

rm(list=ls()[! ls() %in% c("Germany","ESA_rec",'layer_2020')])

#-----------------------------------------------------------------------------#
# E) Distribution ground-truth samples
#-----------------------------------------------------------------------------#

# load the samples and prepare the classes
samples<-read_sf('C:/Users/cb58hypi/Desktop/BiodiversityInformatics/data/samples/Germany.shp') # change to your path

samples<-samples %>%
  mutate(class = ifelse(class == 'cropland', 'cropland', 
  ifelse(class == 'forest', 'forest',
  'other')))

samples = samples[samples$year==2020,] # select only the samples in year 2020 when the validation will be done

# subset and look at the spatial distribution
subset = samples %>% group_by(class) %>% slice_sample(n=500) # select 500 random samples per class

coordinates = as.data.frame(st_coordinates(subset))
names(coordinates)[1]<-'long'
names(coordinates)[2]<-'lat'

ggplot() +
  geom_sf(data = Germany, fill=NA) +
  geom_point(aes(coordinates$lon, coordinates$lat, color = 'green'), size = 1, pch=16) +
  labs(x = "Longitude", y = "Latitude") + theme (legend.position = 'none') 


rm(list=ls()[! ls() %in% c("Germany","ESA_rec",'layer_2020','subset')])

#-----------------------------------------------------------------------------#
# F) F1
#-----------------------------------------------------------------------------#

# forest accuracy
class<-subset[subset$class=='forest',]
other<-subset[subset$class=='other',]
file<-rbind(class,other)

# extract classes from the layer 2020
coordinates<-as.matrix(st_coordinates(file))
cells<-as.data.frame(cellFromXY(layer_2020, coordinates))
names(cells)[1]<-'cell_ID'
cells$class<-as.numeric(terra::extract(layer_2020,cells$cell_ID))

db = cbind(cells,file)
names(db)[2] = 'class_map'  

# find true positives, false positives and false negatives
forest_accuracy = db %>%
  mutate(type = ifelse((class_map == 10 & class== 'forest'), 'TP', 
    ifelse((class_map== 10 & class!= 'forest'),'FP',
      ifelse((class_map!= 10 & class== 'forest'),'FN','TN'))))

forest = forest_accuracy[!(forest_accuracy$type=='TN'),]

forest<-forest %>%
  group_by(type) %>%
  summarise(count=n())

forest = forest[!is.na(forest$type),]

# calculate F1
P = forest$count[forest$type=='TP']/(forest$count[forest$type=='TP']+forest$count[forest$type=='FP'])

R = forest$count[forest$type=='TP']/(forest$count[forest$type=='TP']+forest$count[forest$type=='FN'])

F1_forest = 2 * (P * R) / (P + R)

F1_forest # record this number 

# make a plot             
db = cbind(forest_accuracy,coordinates)             
names(db)[8] = 'long'
names(db)[9] = 'lat'
db = db[!(db$type=='TN'),]

db = db %>%
  mutate(accuracy = ifelse(type == 'FN', 'wrong', 
                           ifelse(type == 'TP', 'correct',
                                  'wrong')))

forest_plot = ggplot() +
  geom_sf(data = Germany, fill=NA) +
  geom_point(aes(db$lon, db$lat, color = db$accuracy), size = 2, pch=16) +
  labs(x = "Longitude", y = "Latitude") + theme_bw() + guides(color = guide_legend(title = "accuracy")) +   ggtitle ('forest')

rm(list=ls()[! ls() %in% c("Germany","ESA_rec",'layer_2020','subset','forest_plot')])

# cropland accuracy
class<-subset[subset$class=='cropland',]
other<-subset[subset$class=='other',]
file<-rbind(class,other)

# extract classes from the layer 2020
coordinates<-as.matrix(st_coordinates(file))
cells<-as.data.frame(cellFromXY(layer_2020, coordinates))
names(cells)[1]<-'cell_ID'
cells$class<-as.numeric(terra::extract(layer_2020,cells$cell_ID))

db = cbind(cells,file)
names(db)[2] = 'class_map'  

# find true positives, false positives and false negatives
cropland_accuracy = db %>%
  mutate(type = ifelse((class_map == 1 & class== 'cropland'), 'TP', 
                       ifelse((class_map== 1 & class!= 'cropland'),'FP',
                              ifelse((class_map!= 1 & class== 'cropland'),'FN','TN'))))

cropland = cropland_accuracy[!(cropland_accuracy$type=='TN'),]

cropland<-cropland %>%
  group_by(type) %>%
  summarise(count=n())

cropland = cropland[!is.na(cropland$type),]

# calculate F1
P = cropland$count[cropland$type=='TP']/(cropland$count[cropland$type=='TP']+cropland$count[cropland$type=='FP'])

R = cropland$count[cropland$type=='TP']/(cropland$count[cropland$type=='TP']+cropland$count[cropland$type=='FN'])

F1_cropland = 2 * (P * R) / (P + R)

F1_cropland # record this number

# make a plot             
db = cbind(cropland_accuracy,coordinates)             
names(db)[8] = 'long'
names(db)[9] = 'lat'
db = db[!(db$type=='TN'),]

db = db %>%
  mutate(accuracy = ifelse(type == 'FN', 'wrong', 
                           ifelse(type == 'TP', 'correct',
                                  'wrong')))
cropland_plot = ggplot() +
  geom_sf(data = Germany, fill=NA) +
  geom_point(aes(db$lon, db$lat, color = db$accuracy), size = 2, pch=16) +
  labs(x = "Longitude", y = "Latitude") + theme_bw() + guides(color = guide_legend(title = "accuracy")) +   ggtitle ('cropland')


ggpubr::ggarrange(cropland_plot,forest_plot, common.legend = TRUE, legend="bottom", align='h',  nrow =1)+
  theme(plot.margin = margin(0,0,0,0, "cm"))


#-----------------------------------------------------------------------------#
# G) Questions: Let's switch to the .Rmd file
#-----------------------------------------------------------------------------#
