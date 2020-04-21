#Script
#install.packages('sdm')
install.packages('dismo')
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
library(sparkline)
library(sdm)
library(rgbif)
library(dismo)
library(raster)
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


?getData

bio <- raster::getData('worldclim',var='bio',res=10) #bioclim data resolution 10m
bio #19 bioclim variables





