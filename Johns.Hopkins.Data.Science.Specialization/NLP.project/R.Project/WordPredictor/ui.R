
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  
##  Date:       09DEC2019
##
##  Step 6
##  Shiny app for wordpredictor
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme=shinytheme("spacelab"),
                         title = 'Word Predictor',
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
          
          sidebarPanel(
                 p("Predict the next word of a sentence."),
                 p("Enter a sentence to see the predicted words.  Remember to press
                   spacebar before last word.")
          ),
    
    # Show a plot of the generated distribution
    mainPanel(
            tabsetPanel(
                    tabPanel("Word Predictor",
                             p(textInput("entry.sentence","Enter sentence and press spacebar:", "There are")),
                             #create scroll down button to choose 3-5 predictions
                             p(    selectInput("number.prediction", "Number of Predictions:",
                                               c("3" = 3,
                                                 "4" = 4,
                                                 "5" = 5))),
                             br(),
                             DT::dataTableOutput("final.results")),
                    
                    tabPanel("About", div(p('This widget predicts words using the ', 
                                            a(href = 'https://www.aclweb.org/anthology/D07-1090.pdf', 
                                              'Stupid Back-Off Algorithm', .noWS = "outside"), 
                                            '.  The sample data (corpus) is a 
                                            collection of the following sources:', 
                                            .noWS = c("after-begin", "before-end"))),
                             tags$ul(
                                     tags$li(p("Partial collection of Gutenberg Project corpus provided by Shibamouli Lahiri: ",
                                               a(href = 'https://web.eecs.umich.edu/~lahiri/gutenberg_dataset.html', 
                                                 'https://web.eecs.umich.edu/~lahiri/gutenberg_dataset.html', .noWS = "outside"))),
                                     tags$li(p("Andrew L. Maas, Raymond E. Daly, Peter T. Pham, 
                                             Dan Huang, Andrew Y. Ng, and Christopher Potts. (2011).",
                                       em("Learning Word Vectors for Sentiment Analysis."),"
                                             The 49th Annual Meeting of the Association for Computational Linguistics (ACL 2011).",
                                       a(href = 'https://ai.stanford.edu/~amaas/data/sentiment/', 'https://ai.stanford.edu/~amaas/data/sentiment/', .noWS = "outside"))
                                       ), 
                                     tags$li(p("Swiftkey provided a dataset which was taken from the following address:",
                                               a(href = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip', 
                                                 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip', .noWS = "outside")))
                                     )
                    )       
            )
            )
)
)
)

