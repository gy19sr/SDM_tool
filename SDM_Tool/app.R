
#Title: Habitat Suitability Tool
#Author: Stuart Ross
#Developed 25/05/2020
#Instructions: 
# 1. highlight the entire libary section and run to install all needed libraries
# 2. once downloads and updates complete remove the below # and highlight installAll and run once. Once everything is install place the # back infront of installall
#installAll

rm(list = ls()) 
if (!require(shiny)) install.packages('shiny')
library(shiny)
#if (!require(tidyverse)) install.packages('tidyverse')
#library(tidyverse)
if (!require(sf)) install.packages('sf')
library(sf)
if (!require(tmap)) install.packages('tmap')
library(tmap)
if (!require(GISTools)) install.packages('GISTools')
library(GISTools)
if (!require(rgdal)) install.packages('rgdal')
library(rgdal)
if (!require(sp)) install.packages('sp')
library(sp)
#if (!require(RColorBrewer)) install.packages('RColorBrewer')
#library(RColorBrewer)
#if (!require(reshape2)) install.packages('reshape2')
#library(reshape2)
if (!require(rpart)) install.packages('rpart')
library(rpart)
if (!require(rpart.plot)) install.packages('rpart.plot')
library(rpart.plot)
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
if (!require(leaflet)) install.packages('leaflet')
library(leaflet)
if (!require(mapview)) install.packages('mapview')
library(mapview)
#if (!require(rJava)) install.packages('rJava')
#library(rJava)
if (!require(parallel)) install.packages('parallel')
library(parallel)
#if (!require(vegan)) install.packages('vegan')
#library(vegan)
if (!require(mnormt)) install.packages('mnormt')
library(mnormt)
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


#################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Create Your Own Habitat Suitability Model"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("Species_name",
                      "species",
                      value = "",
                      placeholder = "Enter species Lower Case"),
            
            textInput("Genus_name",
                      "Genus",
                      value = "",
                      placeholder = "Enter Genus Upper Case")
        ),
        # Main panel for displaying outputs
        mainPanel(
            
            # Output: Tabset
            tabsetPanel(type = "tabs",
                        
                        #Occurence Tab
                        tabPanel("Occurance", 
                                 "Number of Recorded Occurances", 
                                 textOutput("occurances"), 
                                 p(),
                                 leafletOutput("Occurence_map")
                        ),
                        
                        #HSM Plot tab
                        tabPanel("HSM", plotOutput("HSM_map")),
                        
                        
                        #Variable Importance
                        tabPanel("variable Importance", 
                                 plotOutput("Var_imp"),
                                 verbatimTextOutput("bio_defs")),
                        
                        #predictive Model 
                        tabPanel("Predictive Model", 
                                 sliderInput ("slider1", label = h3("Adjust the slider to see suitable Habitat in _ years"), 
                                              min = 50, max = 70, value = 50, step = 20), 
                                 plotOutput("pred_model"))
            )
            
            
        )
    )
)


#######################################################################################


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    #### map viewer ####
    
    output$Occurence_map <-  renderLeaflet({
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        sp <- gbif(genus_name,species_name,download = T, geo=T,sp=F, end=200)
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = sp)
    })
    
    
    #### Text for variables ####
    
    output$bio_defs <- renderText({
        paste("BIO 2 = Mean Diurnal Range", 
              "BIO 3 = Isothermality",
              "BIO 8 = Mean Temperature of Wettest Quarter",
              "BIO 9 = Mean Temperature of Driest Quarter",
              "BIO 13 = Precipitation of Wettest Month",
              "BIO 14 = Precipitation of Driest Month",
              "BIO 15 = Precipitation Seasonality",
              "BIO 18 = Precipitation of Warmest Quarter",
              "BIO 19 = Precipitation of Coldest Quarter",
              sep="\n")
    })
    
    #### Occurence Text ####
    
    output$occurances <- renderText({
        
        # generate species data
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        
        # write down the number of occurances downloaded
        sp_occurences <- gbif(genus_name,species_name,download = F) #check occurances
        sp_occurences
    })
    
    
    #### HSM Plot ####
    
    output$HSM_map <- renderPlot({
        
        # generate species data
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        sp <- gbif(genus_name,species_name,download = T, geo=T,sp=F, end=200)
        sp$species <- 1    #add new column species
        sp <- sp[,c('lon','lat','species')]    #remove uneeded columns
        w <- which(is.na(sp$lon)) 
        matrixc <- cbind(w, c(1))
        sp <-sp[-matrixc,]
        coordinates(sp) <- ~lon + lat   #remove uneeded columns
        #plot the points
        proj4string(sp) <-projection(raster()) #set projection by making empty raster and assigning its projection
        bio <- raster::getData('worldclim',var='bio',res=10) 
        v1 <- vifstep(bio)
        biom <- exclude(bio,v1)
        d <-sdmData(species~., sp, predictors = biom, bg = list(n=500)) #Prepare the species data
        m <-sdm(species~., d, methods=c('rf'),
                replication=c('boot'),n=2) 
        p <- predict(m, biom, 'predictions.img', overwrite=T)
        raster::plot(p[[1]])
    })   
    
    #### Variable Importance Plot ####
    
    output$Var_imp <- renderPlot({
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        sp <- gbif(genus_name,species_name,download = T, geo=T,sp=F, end=200)
        sp$species <- 1    #add new column species
        sp <- sp[,c('lon','lat','species')]    #remove uneeded columns
        w <- which(is.na(sp$lon)) 
        matrixc <- cbind(w, c(1))
        sp <-sp[-matrixc,]
        coordinates(sp) <- ~lon + lat   #remove uneeded columns
        #plot the points
        proj4string(sp) <-projection(raster()) #set projection by making empty raster and assigning its projection
        bio <- raster::getData('worldclim',var='bio',res=10) 
        v1 <- vifstep(bio)
        biom <- exclude(bio,v1)
        d <-sdmData(species~., sp, predictors = biom, bg = list(n=500)) #Prepare the species data
        m <-sdm(species~., d, methods=c('rf'),
                replication=c('boot'),n=2) 
        VI <- getVarImp(m, 1)
        plot(VI)
    })
    
    #### Predictive Model ####
    
    output$pred_model <- renderPlot ({
        genus_name <- input$Genus_name
        species_name <- input$Species_name
        sp <- gbif(genus_name,species_name,download = T, geo=T,sp=F, end=200)
        sp$species <- 1    #add new column species
        sp <- sp[,c('lon','lat','species')]    #remove uneeded columns
        w <- which(is.na(sp$lon)) 
        matrixc <- cbind(w, c(1))
        sp <-sp[-matrixc,]
        coordinates(sp) <- ~lon + lat
        #plot the points
        proj4string(sp) <-projection(raster()) #set projection by making empty raster and assigning its projection
        bio <- raster::getData('worldclim',var='bio',res=10) 
        v1 <- vifstep(bio)
        biom <- exclude(bio,v1)
        d <-sdmData(species~., sp, predictors = biom, bg = list(n=500))
        m <-sdm(species~., d, methods=c('rf'),
                replication=c('boot'),n=2) 
        scale_num <- input$slider1
        biof <- raster::getData('CMIP5',var='bio',res=10,rcp=85,year=scale_num,model='AC')
        names(biof) <-names(bio)
        pf <- predict(m, biof, 'predictionsf.img')
        enf <- calc(pf, mean)
        
        enf <- ensemble(m, biof, 'ensf.img', 
                        setting=list(method='weights', stat="TSS", opt=2))
        raster::plot(enf)
        
    })
    
    
    
    
}




# Run the application 
shinyApp(ui = ui, server = server)

