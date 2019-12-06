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

#NEXT FIGURE OUT FONT SIZE

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(
          h1("WordPredictor", align = "center")
          ),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
          column(3,
                 p("The widget predicts the next word of a sentence."),
                 p("Enter a sentence to see the predicted words.")
          ),
    
    # Show a plot of the generated distribution
    column(9,
            tabsetPanel(
                    tabPanel("Word Predictor",
                             p(textInput("entry.sentence","Enter sentence:", "There are")),
                             #create scroll down button to choose 3-5 predictions
                             p(textInput("number.prediction","Number of predictions:","5")),
                             br(),
                             DT::dataTableOutput("final.results")),
                    
                    tabPanel("About", div(p('This widget predicts words using the ', 
                                            a(href = 'https://www.aclweb.org/anthology/D07-1090.pdf', 
                                              'Stupid Back-Off Algorithm', .noWS = "outside"), 
                                            '.  The sample data (corpus) is a 
                                            collection of the following sources:', 
                                            .noWS = c("after-begin", "before-end"))),
                             tags$ul(
                                     tags$li("Gutenberg credit here"), 
                                     tags$li("Stanford Movies"), 
                                     tags$li("Swiftkey")
                                     )
                    )       
            )
            )
)
)
)

