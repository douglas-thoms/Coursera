# Define server logic required to draw a histogram
server <- function(input, output) {
        
        
        options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
        
        census_variables <- c("v_CA16_5051","v_CA16_5054","v_CA16_5057","v_CA16_5060","v_CA16_5063",
                              "v_CA16_5066","v_CA16_5069","v_CA16_5072","v_CA16_5075",
                              "v_CA16_5078","v_CA16_5081","v_CA16_5084","v_CA16_5087",
                              "v_CA16_5090","v_CA16_5093")
        
        census_data <- get_census(dataset='CA16', regions=list(C="01"), labels = "short", 
                                  vectors=census_variables, level='PR', geo_format='sf')
        
        output$distPlot <- renderPlot({
                # generate bins based on input$bins from ui.R
                x    <- faithful[, 2] 
                bins <- seq(min(x), max(x), length.out = input$bins + 1)
                
                # draw the histogram with the specified number of bins
                hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
}