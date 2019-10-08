library(shiny)

#Data set
#cancenus
#mode - immigrants - language /ethniticity/age inputs - what is mode in map for population?

#What to display?
#lm actual vs predict?

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel("Education Levels in Canada"),
        
        #input
        selectInput("variable", "Education Level:",
                    c("Total - Highest certificate, diploma or degree for the population aged 15 years" = "v_CA16_5051",
                      "No certificate, diploma or degree" = "v_CA16_5054",
                      "Secondary (high) school diploma or equivalency certificate" = "v_CA16_5057",
                      "Postsecondary certificate, diploma or degree" = "v_CA16_5060",
                      "Bachelor's degree" = "v_CA16_5081",
                      "Master's degree" = "v_CA16_5090",
                      "Earned doctorate" = "v_CA16_5093")),
        
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

