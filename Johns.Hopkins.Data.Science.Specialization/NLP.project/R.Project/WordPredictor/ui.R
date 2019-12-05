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
                    tabPanel("Word Predictor",
                             h5(textInput("entry.sentence","Enter sentence:")),
                             h5(textInput("number.prediction","Number of predictions:","5")),
                             br(),
                             DT::dataTableOutput("final.results")),
                    
                    tabPanel("About", h4(strong("Uses Stupid Back-Off Algorithm: Put a string of words in and get a sentence.")),
                                        br(), 
                                        h4("Gutenberg credit here"),
                                        br(),
                                        h4("corpus explanation"))
            )
            )
)
)
)

