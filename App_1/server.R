#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
#check if package then only install if neccessary
#
if (!require(shiny)) install.packages('shiny')
library(shiny)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
#library(sf)
#library(tmap)
#library(GISTools)
#library(rgdal)
if (!require(sp)) install.packages('sp')
library(sp)
#library(RColorBrewer)
#library(reshape2)
#library(rpart)
#library(rpart.plot)
#library(visNetwork)
if (!require(caret)) install.packages('caret')
library(caret)
#library(randomForest)
if (!require(ranger)) install.packages('ranger')
library(ranger)
if (!require(usdm)) install.packages('usdm')
library(usdm)
if (!require(sparkline)) install.packages('sparkline')
library(sparkline)
if (!require(sdm)) install.packages('sdm')
library(sdm)
if (!require(rgbif)) install.packages('rgbif')
library(rgbif)
if (!require(dismo)) install.packages('dismo')
library(dismo)
if (!require(raster)) install.packages('raster')
library(raster)
#library(mapview)
#library(rJava)
#library(parallel)
#library(vegan)
if (!require(mnormt)) install.packages('mnormt')
library(mnormt)
if (!require(raster)) install.packages('raster')
library(raster)
if (!require(R6)) install.packages('R6')
library(R6)  
if (!require(Rcpp)) install.packages('Rcpp')
library(Rcpp)
if (!require(purrr)) install.packages('purrr')
library(purrr)
if (!require(permute)) install.packages('permute')
library(permute)
if (!require(kernlab)) install.packages('kernlab')
library(kernlab)
#installAll()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$occurances <- renderText({

        # generate species data
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        sp <- gbif(genus_name,species_name,download = T, geo=T,sp=F) #check occurances
       
        # write down the number of occurances downloaded
        sp_occurences <- gbif(genus_name,species_name,download = F) #check occurances
        sp_occurences
    })
    
    output$HSM_map <- renderPlot({
        
        # generate species data
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        sp <- gbif(genus_name,species_name,download = T, geo=T,sp=F)
        w <- which(is.na(sp$lon))
        sp <-sp[-w,]
        sp$species <- 1    #add new column species
        sp <- sp[,c('lon','lat','species')]    #remove uneeded columns
        coordinates(sp) <- ~lon + lat
        #plot the points
        proj4string(sp) <-projection(raster()) #set projection by making empty raster and assigning its projection
        bio <- raster::getData('worldclim',var='bio',res=10) 
        v1 <- vifstep(bio)
        biom <- exclude(bio,v1)
        d <-sdmData(species~., sp, predictors = biom, bg = list(n=1000))
        m <-sdm(species~., d, methods=c('rf'),
                replication=c('boot'),n=3) 
        p <- predict(m, biom, 'predictions.img', overwrite=T)
        raster::plot(p[[1]])
    })    

})

