#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

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
))
