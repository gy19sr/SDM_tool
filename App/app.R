
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
library(R6)  
library(Rcpp)
library(purrr)
library(permute)
library(kernlab)


server <- function(input, output) {
  
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
  
}



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
                "Genius",
                value = "",
                placeholder = "Enter Genus Upper Case")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("occurances"),
      plotOutput("HSM_map")
    )
  )
)



