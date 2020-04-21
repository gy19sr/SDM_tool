#Script
#install.packages('sdm')
#install.packages('dismo')
rm(list = ls()) #clear work space
library(tidyverse)
library(sf)
library(tmap)
library(GISTools)
library(rgdal)
library(sp)
library(RColorBrewer)
library(reshape2)
library(rpart)
library(rpart.plot)
library(visNetwork)
library(caret)
library(randomForest)
library(ranger)
library(usdm)
library(sparkline)
library(sdm)
library(rgbif)
library(dismo)
library(raster)
library(mapview)
#installAll()


###### Set your working directory ######

#setwd("C:/Users/stuar/OneDrive/Documents/onlinecourses/Ecology/Ecological_Data_modelling")  
# If you're working in an R project, skip this step

############  check and load data  ##############
#sp = spatial point, geo means give coordinates
#?gbif 
#capitalisation matters
sp <- gbif("Lynx","pardinus",download = f) #check occurances
sp <- gbif("Lynx","pardinus",download = T, geo=T,sp=F) # Download 


############## data cleaning ##############

#remove NA values in lat and lon
w <- which(is.na(sp$lon))
sp <-sp[-w,]
#m <- which(is.na(sp$lat)) #seems to remove all observations
#sp <-sp[-m,]



sp$species <- 1    #add new column species
sp <- sp[,c('lon','lat','species')]    #remove uneeded columns
head(sp)
coordinates(sp) <- ~lon + lat #make species into spatial points
class(sp)
head(sp)  #this is presence only data


########### predictor variables #################
#test

?getData
#bio clim
bio <- raster::getData('worldclim',var='bio',res=10) #bioclim data resolution 10m
bio #19 bioclim variables
#https://worldclim.org/data/bioclim.html for 19 variables


v1 <- vifstep(bio) #checking vif for collinearity
v1

biom <- exclude(bio,v1)
biom #9 remaining variables


######### viewing a basic map #############
plot(biom[[1]]) #plot predictor variables

points(sp, cex=0.5,pch=16) #plot points of data

proj4string(sp)  # check projection (should be undefined)
proj4string(sp) <-projection(raster()) #set projection by making empty raster and assigning its projection
mapview(sp)

########### model prep ##########
head(sp)
d <-sdmData(species~., sp, predictors = biom) #species should be lone column, training = sp, then predictors
d
#currently only Presence only data - so create pseudo background data
? sdmData
d <-sdmData(species~., sp, predictors = biom, bg = list(n=1000)) #1000 bg points
d


############# Running the model ###########

#train and test (still to add)
#need to select methods
#getmethodNames()
m <-sdm(species~., d, methods=c('glm','svm','rf','brt','mars','maxent'),
  replication=c('boot'),n=5) #set number of bootstraping
  #number of replications and rep method can be changed

