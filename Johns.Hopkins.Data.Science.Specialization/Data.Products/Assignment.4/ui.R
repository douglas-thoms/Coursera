library(shiny)

#Data set
#cancenus
#mode - immigrants - language /ethniticity/age inputs - what is mode in map for population?

#What to display?
#lm actual vs predict?

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel("Old Faithful Geyser Data"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30)
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        plotOutput("distPlot")
                )
        )
)

