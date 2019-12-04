#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# https://shiny.rstudio.com/articles/layout-guide.html

#https://shiny.rstudio.com/articles/tabsets.html

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("WordPredictor"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
          column(3,
                 h4("Put a string of words in and get a sentence.") #bold this
          ),
    
    # Show a plot of the generated distribution
    column(9,
            tabsetPanel(
                    tabPanel("Word Predictor", dataTableOutput("final.results"),h5(textInput("entry.sentence","Predicted sentence is:"))),
                    tabPanel("About", h4("Uses Stupid Back-Off Algorithm: Put a string of words in and get a sentence.<br>
                                         Gutenberg credit here"))
            )
            )
)
)
)

