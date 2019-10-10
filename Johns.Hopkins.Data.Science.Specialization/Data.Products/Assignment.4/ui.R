library(cancensus)
library(sf)
library(leaflet)
library(dplyr)

#Data set
#cancenus
#mode - immigrants - language /ethniticity/age inputs - what is mode in map for population?

#What to display?
#lm actual vs predict?

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel("Education Levels in Canada"),
        
        sidebarPanel(
                #input for education level
                selectInput("variable", "Highest Education Level:",
                            c("No certificate, diploma or degree" = "v_CA16_5054",
                              "Secondary (high) school diploma or equivalency certificate" = "v_CA16_5057",
                              "Postsecondary certificate, diploma or degree" = "v_CA16_5060",
                              "Bachelor's degree" = "v_CA16_5081",
                              "Master's degree" = "v_CA16_5090",
                              "Earned doctorate" = "v_CA16_5093")),
                #input for Region level
                selectInput("area", "Region Level:",
                            c("Province" = "PR",
                              "Census Division" = "CD")),
                
                #input for Region level
                selectInput("year", "Census Year:",
                            c("2001 Canada Census" = "CA01",
                              "2006 Canada Census" = "CA06",
                              "2011 Canada Census and NH" = "CA11",
                              "2016 Canada Census" = "CA16"))
                ),
        
        #main panel setup
                mainPanel(
                        # Output: Tabset w/ plot, summary, and table ----
                        tabsetPanel(type = "tabs",
                                    tabPanel("Map", leafletOutput("map")),
                                    tabPanel("Documentation", htmlOutput("text"))
                                    )
                        )
)
