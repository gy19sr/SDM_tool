#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
    
    output$occurances_map <- renderPlot({
        
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
        raster::plot(sp)
    })    

})
