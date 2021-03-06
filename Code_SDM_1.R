#Script
#install.packages('sdm')
#install.packages('dismo')
#installAll()
#install.packages('rJava')
#install.packages('rsconnect')

rm(list = ls()) #clear work space
library(shiny)
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
library(rJava)
library(parallel)
library(vegan)
library(mnormt)
library(psych)

?mainPanel

#if (!require(abind)) install.packages('abind')
#library(abind)

?abind
###### Set your working directory ######

#setwd("C:/Users/stuar/OneDrive/Documents/onlinecourses/Ecology/Ecological_Data_modelling")  
# If you're working in an R project, skip this step

############  check and load data  ##############
#sp = spatial point, geo means give coordinates
?gbif 
#capitalisation matters
#Camelus bactrianus
# Microcebus danfossi
#Varecia rubra
sp <- gbif("Hapalemur","aureus",download = F) #check occurances
sp <- gbif("Canis","lupus",download = F)
sp
sp <- gbif("Varecia","rubra",download = T, geo=T,sp=F,end=30) # Download 
sp <- gbif("Camelus","bactrianus",download = T, geo=T,sp=F,end=30)

#Panthera pardus
length(sp)

??shiny
sp <- gbif("Panthera","pardus",download = T, geo=T,sp=F,end=1000)
head(sp)

############## data cleaning ##############

#remove NA values in lat and lon
w <- which(is.na(sp$lon))
sp <-sp[-w,]
#m <- which(is.na(sp$lat)) #seems to remove all observations
#sp <-sp[-m,]



sp$species <- 1    #add new column species
sp <- sp[,c('lon','lat','species')]    #remove uneeded columns
#head(sp)
coordinates(sp) <- ~lon + lat #make species into spatial points
#class(sp)
#head(sp)  #this is presence only data
raster::plot(sp)


########### predictor variables #################
#test

?getData
#bio clim
bio <- raster::getData('worldclim',var='bio',res=10) #bioclim data resolution 10m
bio #19 bioclim variables
## https://worldclim.org/data/bioclim.html for 19 variables


v1 <- vifstep(bio) #checking vif for collinearity
v1
#can set threshold as well

biom <- exclude(bio,v1)
biom #9 remaining variables


######### viewing a basic map #############
raster::plot(biom[[1]]) #plot predictor variables
?plot
points(sp, cex=0.5,pch=16) #plot points of data

?addMarkers

proj4string(sp)  # check projection (should be undefined)
proj4string(sp) <-projection(raster()) #set projection by making empty raster and assigning its projection
mapview(sp)

addMarkers(data = sp)

########### model prep ##########
head(sp)
d <-sdmData(species~., sp, predictors = biom) #species should be lone column, training = sp, then predictors
d
#currently only Presence only data - so create pseudo background data
#? sdmData
d <-sdmData(species~., sp, predictors = biom, bg = list(n=1000)) #1000 background points
#bg points randomly selct it across the study area
#can make it distributed geographical area 'gRandom'
d
###PA needs some work -- what do I do with no absence

############# Fitting and Running the model ###########

#train and test (still to add)
#need to select methods
#getmethodNames()
m <-sdm(species~., d, methods=c('rf'),
        replication=c('boot'),n=3) 
#m <-sdm(species~., d, methods=c('glm','svm','rf'),
#  replication=c('boot'),n=2) #set number of bootstraping
  #number of replications and rep method can be changed

m #report of how fitted 
#large inflation in estimated absence as it was pseudo absecene
#far better with actual absence data
#obviously over fit



??Plot

#predict probably current distribution
p <- predict(m, biom, 'predictions.img', overwrite=T) #can add mean =T to do ave model result
p

raster::plot(p[])
raster::plot(p[[1]]) #plot differenct model results
raster::plot(p[[]])
gui(m)


#getAnywhere(gui)
#sdm:::gui
#showMethods(gui)
#getMethod("gui","sdmModels")
#getAnywhere(shiny)

#getMethod("getVarImp","sdm")
#??sdm




en <- ensemble(m, biom, 'ens.img',
    setting=list(method='weights',stat="TSS",opt=2))
#higher weights given to methods with higher accuracy
#if use stat="TSS",opt=2 (number refers to row in gui table threshold base)
#when do stat="AUC" no opt needed as independent

 
########### future predictions #############
biof <- raster::getData('CMIP5',var='bio',res=10,rcp=85,year=70,model='AC') #prediction for yeatd in the future

names(biof)
names(bio)
names(biof) <-names(bio) #correct names
names(biof)

pf <- predict(m, biof, 'predictionsf.img')

enf <- calc(pf, mean)

enf <- ensemble(m, biof, 'ensf.img', 
                setting=list(method='weights', stat="TSS", opt=2))

plot(enf) #future ensemble
plot(stack(en, enf)) #compare current and future


mapview(stack(en, enf))

# ^^^^ this is all really a habitat suitability model
#I think to make it a SDM you need to know accuracy

################# calculate the diffrence between the maps and trhough time ########################################

ch <- enf - en
plot(ch) # + gained suitability over time - lost suitability over time

################# compare presence absence #########################
ev <- getEvaluation(m, stat = c('AUC', 'TSS', 'threshold'), opt=2) #use threshold tab in gui look at model and criteria for number
ave <- mean(ev$threshold)
pa <- raster(en)
pa[] <- ifelse(en[] >= ave, 1, 0) #put in mean threshold if higher 1 if lower zero

plot(pa)


# future presence absence
paf <- raster(enf)
paf[] <- ifelse(enf[] >= ave, 1, 0) #put in mean threshold if higher 1 if lower zero

plot(paf)



#difference map of now and future
pac <- paf -pa
cl <- colorRampPalette(c('red','white','darkgreen'))
plot(pac,col=cl(3)) #extinction persistance and colinization


###########Response curve ###############
rcurve(m) #range over species survive



######################################### Variable Importance test ###########################
data.frame(name = names(rfFit$variable.importance),
           value = rescale(rfFit$variable.importance, c(0,100))) %>%
  arrange(desc(value)) %>%
  ggplot(aes(reorder(name, value), value)) +
  geom_col() + coord_flip() + xlab("") +
  theme(axis.text.y = element_text(size = 7))

rfFit$variable.importance





